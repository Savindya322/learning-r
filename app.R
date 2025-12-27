# A Shiny app for X-bar and R control charts (Phase I / Phase II)
# Usage:
# 1) Install required packages: install.packages(c("shiny","qcc","ggplot2","dplyr","DT","readr"))
# 2) Save this file as app.R and run: shiny::runApp("app.R")
#
# Features:
# - Upload CSV file (or load example data)
# - Choose measurement column and subgrouping (by column or by sequential subgroup size)
# - Define Phase I (rows or groups) to estimate control limits
# - Apply Phase II (remaining data) to monitor using Phase I limits
# - Shows X-bar and R charts for Phase I and Phase II, summary table of out-of-control points

library(shiny)
library(qcc)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)

ui <- fluidPage(
  titlePanel("X-bar & R Control Charts (Phase I / Phase II) - Quality Assurance"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file", accept = c(".csv")),
      actionButton("load_example", "Load example dataset"),
      hr(),
      uiOutput("var_select_ui"),
      hr(),
      radioButtons("group_mode", "Group data by:",
                   choices = c("Subgroup column" = "col", "Sequential chunking (fixed subgroup size)" = "size"),
                   selected = "col"),
      uiOutput("group_input_ui"),
      hr(),
      helpText("Define Phase I data used to estimate control limits:"),
      radioButtons("phase_select_mode", "Select Phase I by:",
                   choices = c("Use a column labeling Phase (e.g., 'phase')" = "col",
                               "Use first K groups as Phase I" = "firstk",
                               "Use row range as Phase I" = "rows"),
                   selected = "firstk"),
      uiOutput("phase_input_ui"),
      hr(),
      actionButton("go", "Compute charts"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data preview", DTOutput("data_tbl")),
        tabPanel("Phase I: X-bar & R", 
                 h4("X-bar chart (Phase I)"),
                 plotOutput("xbar_phase1_plot", height = "350px"),
                 h4("R chart (Phase I)"),
                 plotOutput("r_phase1_plot", height = "350px"),
                 verbatimTextOutput("phase1_stats")
        ),
        tabPanel("Phase II: Monitoring",
                 h4("X-bar chart (Phase II using Phase I limits)"),
                 plotOutput("xbar_phase2_plot", height = "350px"),
                 h4("R chart (Phase II using Phase I limits)"),
                 plotOutput("r_phase2_plot", height = "350px"),
                 verbatimTextOutput("phase2_stats")
        ),
        tabPanel("Out-of-Control Points",
                 DTOutput("ooc_tbl")
        ),
        tabPanel("Help / Notes",
                 tags$h4("How to use"),
                 tags$ul(
                   tags$li("Dataset should contain a numeric measurement column."),
                   tags$li("If subgroup column is used, each subgroup should have the same subgroup size for R chart validity."),
                   tags$li("If subgroup sizes vary, the app will warn; consider using s-charts or aggregating to constant subgroup size.")
                 ),
                 tags$h4("Example dataset format"),
                 tags$pre('subgroup,measurement,phase
1,10.1,I
1,9.9,I
2,10.5,II
... ')
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  # Reactive dataset
  data_raw <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch({
      read_csv(input$file$datapath, show_col_types = FALSE)
    }, error = function(e) {
      showNotification("Error reading CSV: ensure it's a valid CSV file", type = "error")
      return(NULL)
    })
    data_raw(df)
  })
  
  observeEvent(input$load_example, {
    # create example dataset: 30 groups, subgroup size 4
    set.seed(123)
    k <- 30
    n <- 4
    groups <- rep(1:k, each = n)
    measurements <- rnorm(k * n, mean = 10, sd = 0.5)
    # mark first 20 groups as Phase I
    phase <- ifelse(groups <= 20, "I", "II")
    df <- data.frame(subgroup = groups, measurement = measurements, phase = phase)
    data_raw(df)
  })
  
  output$data_tbl <- renderDT({
    df <- data_raw()
    req(df)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$var_select_ui <- renderUI({
    df <- data_raw()
    if (is.null(df)) {
      tagList(
        helpText("Upload a CSV to begin or load example dataset.")
      )
    } else {
      num_vars <- names(df)[sapply(df, is.numeric)]
      tagList(
        selectInput("measure_var", "Measurement (numeric) column", choices = num_vars, selected = num_vars[1]),
        helpText("Choose the numeric column to chart.")
      )
    }
  })
  
  output$group_input_ui <- renderUI({
    df <- data_raw()
    if (is.null(df)) return(NULL)
    if (input$group_mode == "col") {
      tagList(
        selectInput("group_col", "Subgroup column (categorical / integer)", choices = names(data_raw()), selected = names(data_raw())[1]),
        helpText("Each unique value will form a subgroup.")
      )
    } else {
      tagList(
        numericInput("subgroup_size", "Subgroup size (n)", value = 4, min = 2, step = 1),
        helpText("Data will be split sequentially into groups of this size.")
      )
    }
  })
  
  output$phase_input_ui <- renderUI({
    df <- data_raw()
    if (is.null(df)) return(NULL)
    switch(input$phase_select_mode,
           "col" = tagList(
             selectInput("phase_col", "Phase column (values like I/II or 1/2)", choices = names(df), selected = names(df)[1]),
             textInput("phase_value_for_I", "Value representing Phase I (exact match)", value = "I")
           ),
           "firstk" = tagList(
             numericInput("phase1_groups", "Number of groups to use as Phase I", value = 20, min = 1, step = 1),
             helpText("If using subgroup column, this refers to number of groups; if using chunking, number of groups as well.")
           ),
           "rows" = tagList(
             numericInput("phase1_row_start", "Phase I start row", value = 1, min = 1, step = 1),
             numericInput("phase1_row_end", "Phase I end row", value = 80, min = 1, step = 1),
             helpText("Rows are 1-based index in the uploaded dataset.")
           )
    )
  })
  
  # Helper: build subgroup list and matrices
  build_groups <- function(df, measure_col, group_mode, group_col=NULL, subgroup_size=NULL) {
    if (group_mode == "col") {
      req(group_col)
      df2 <- df %>% mutate(.sg = as.character(.data[[group_col]])) %>%
        select(.sg, !!sym(measure_col))
      groups <- split(df2[[measure_col]], df2$.sg)
      names(groups) <- names(split(df2[[measure_col]], df2$.sg))
    } else {
      req(subgroup_size)
      n <- as.integer(subgroup_size)
      vals <- df[[measure_col]]
      # drop trailing incomplete group (warn)
      full_groups <- floor(length(vals) / n)
      if (full_groups == 0) stop("Not enough rows to form even one subgroup")
      groups <- split(vals[1:(full_groups * n)], rep(1:full_groups, each = n))
      names(groups) <- as.character(1:full_groups)
    }
    groups
  }
  
  # Convert list of groups to matrix (rows = subgroup, cols = obs in subgroup) as required by qcc:
  groups_to_matrix <- function(groups) {
    sizes <- sapply(groups, length)
    if (length(unique(sizes)) != 1) {
      # unequal subgroup sizes
      return(list(mat = NULL, sizes = sizes))
    }
    n <- unique(sizes)
    mat <- matrix(unlist(groups), ncol = n, byrow = TRUE)
    list(mat = mat, sizes = sizes)
  }
  
  # Reactive compute when "go" pressed
  results <- eventReactive(input$go, {
    df <- data_raw()
    req(df, input$measure_var)
    # build groups
    groups <- tryCatch({
      if (input$group_mode == "col") {
        build_groups(df, input$measure_var, "col", group_col = input$group_col)
      } else {
        build_groups(df, input$measure_var, "size", subgroup_size = input$subgroup_size)
      }
    }, error = function(e) {
      showNotification(paste("Error grouping data:", e$message), type = "error")
      return(NULL)
    })
    req(groups)
    
    gm <- groups_to_matrix(groups)
    if (is.null(gm$mat)) {
      showNotification("Subgroups have unequal sizes. R-chart requires equal subgroup sizes. You can still compute X-bar with variable sizes but R-chart will be skipped.", type = "warning", duration = 6)
    }
    
    # Determine which groups are Phase I
    n_groups <- length(groups)
    phase1_idx <- NULL
    if (input$phase_select_mode == "col") {
      req(input$phase_col, input$phase_value_for_I)
      # map groups back to original identifiers (works only when grouping by column)
      if (input$group_mode != "col") {
        showNotification("Phase column selection requires grouping by a subgroup column", type = "error")
        return(NULL)
      }
      # determine unique subgroup order as in build_groups
      grp_ids <- names(groups)
      # reconstruct mapping: unique values in same order as split used
      # split used df %>% mutate(.sg = as.character(.data[[group_col]])) so the order is by appearance
      unique_ids <- grp_ids
      # Build vector of phase flags by subgroup id: look at first row of each subgroup in original df
      # Simpler: create a mapping of subgroup id to phase value by taking first occurrence
      mapping <- df %>% mutate(.sg = as.character(.data[[input$group_col]])) %>%
        group_by(.sg) %>% summarize(phase_value = as.character(first(.data[[input$phase_col]])), .groups = "drop")
      mapping <- mapping %>% arrange(match(.sg, unique_ids))
      phase1_idx <- which(mapping$phase_value == input$phase_value_for_I)
      if (length(phase1_idx) == 0) {
        showNotification("No groups matched the provided Phase I value", type = "error")
        return(NULL)
      }
    } else if (input$phase_select_mode == "firstk") {
      req(input$phase1_groups)
      k <- as.integer(input$phase1_groups)
      if (k < 1) {
        showNotification("Phase I groups must be >= 1", type = "error")
        return(NULL)
      }
      if (k > n_groups) {
        showNotification("Phase I groups exceed total groups", type = "error")
        return(NULL)
      }
      phase1_idx <- 1:k
    } else if (input$phase_select_mode == "rows") {
      req(input$phase1_row_start, input$phase1_row_end)
      start <- as.integer(input$phase1_row_start); end <- as.integer(input$phase1_row_end)
      if (start < 1 || end < start || end > nrow(df)) {
        showNotification("Invalid row range for Phase I", type = "error")
        return(NULL)
      }
      # Map which groups have any row in that range
      if (input$group_mode == "col") {
        # determine for each subgroup whether its rows intersect the specified row range
        # find first row index of each subgroup and last
        grp_rows <- df %>% mutate(.sg = as.character(.data[[input$group_col]])) %>%
          mutate(.row = row_number()) %>%
          group_by(.sg) %>%
          summarize(row_min = min(.row), row_max = max(.row), .groups = "drop")
        grp_rows <- grp_rows %>% arrange(match(.sg, names(groups)))
        phase1_idx <- which(!(grp_rows$row_max < start | grp_rows$row_min > end))
        if (length(phase1_idx) == 0) {
          showNotification("No groups intersect the chosen row range", type = "error")
          return(NULL)
        }
      } else {
        # sequential chunking: groups are sequential; compute which groups fall into row range
        group_size <- as.integer(input$subgroup_size)
        grp_numbers <- seq_along(groups)
        grp_row_start <- (grp_numbers - 1) * group_size + 1
        grp_row_end <- grp_numbers * group_size
        phase1_idx <- which(!(grp_row_end < start | grp_row_start > end))
        if (length(phase1_idx) == 0) {
          showNotification("No groups intersect the chosen row range", type = "error")
          return(NULL)
        }
      }
    }
    
    # Build matrices for Phase I and Phase II (if possible)
    # For equal-size subgroups:
    mat <- gm$mat
    sizes <- gm$sizes
    equal_size <- !is.null(mat)
    
    if (equal_size) {
      # mat rows correspond to groups in the same order as names(groups)
      phase1_mat <- mat[phase1_idx, , drop = FALSE]
      phase2_idx <- setdiff(seq_len(nrow(mat)), phase1_idx)
      phase2_mat <- if (length(phase2_idx) > 0) mat[phase2_idx, , drop = FALSE] else NULL
    } else {
      # unequal sizes -> we will still create a list for qcc with vector of group means; X-bar with variable sizes possible via qcc by providing vector of group means and sizes
      group_means <- sapply(groups, mean)
      group_ranges <- sapply(groups, function(x) if (length(x) > 1) max(x) - min(x) else 0)
      phase1_means <- group_means[phase1_idx]
      phase2_means <- group_means[setdiff(seq_along(groups), phase1_idx)]
      phase1_sizes <- sizes[phase1_idx]
      phase2_sizes <- sizes[setdiff(seq_along(groups), phase1_idx)]
      phase1_mat <- list(means = phase1_means, sizes = phase1_sizes, ranges = group_ranges[phase1_idx])
      phase2_mat <- list(means = phase2_means, sizes = phase2_sizes, ranges = group_ranges[setdiff(seq_along(groups), phase1_idx)])
    }
    
    # Use qcc for calculations
    res <- list(
      groups = groups,
      equal_size = equal_size,
      sizes = sizes,
      phase1_idx = phase1_idx,
      phase2_idx = setdiff(seq_along(groups), phase1_idx)
    )
    
    if (equal_size) {
      n <- ncol(phase1_mat)
      # X-bar Phase I
      q_xbar_p1 <- tryCatch({
        qcc(data = phase1_mat, type = "xbar", plot = FALSE)
      }, error = function(e) {
        showNotification(paste("qcc X-bar Phase I error:", e$message), type = "error")
        return(NULL)
      })
      q_r_p1 <- tryCatch({
        qcc(data = phase1_mat, type = "R", plot = FALSE)
      }, error = function(e) {
        showNotification(paste("qcc R Phase I error:", e$message), type = "error")
        return(NULL)
      })
      # Phase II: monitoring using newdata
      q_xbar_p2 <- NULL; q_r_p2 <- NULL
      if (!is.null(phase2_mat) && nrow(phase2_mat) > 0) {
        # qcc can take newdata argument. To get chart with Phase I limits applied to Phase II set newdata to phase2_mat
        q_xbar_p2 <- tryCatch({
          qcc(data = phase1_mat, type = "xbar", newdata = phase2_mat, plot = FALSE)
        }, error = function(e) {
          showNotification(paste("qcc X-bar Phase II error:", e$message), type = "error")
          NULL
        })
        q_r_p2 <- tryCatch({
          qcc(data = phase1_mat, type = "R", newdata = phase2_mat, plot = FALSE)
        }, error = function(e) {
          showNotification(paste("qcc R Phase II error:", e$message), type = "error")
          NULL
        })
      }
      res$q_xbar_p1 <- q_xbar_p1
      res$q_r_p1 <- q_r_p1
      res$q_xbar_p2 <- q_xbar_p2
      res$q_r_p2 <- q_r_p2
    } else {
      # Unequal sizes: compute X-bar chart using group means and sizes (qcc can accept means with sizes)
      q_xbar_p1 <- tryCatch({
        qcc(data = phase1_mat$means, sizes = phase1_mat$sizes, type = "xbar", plot = FALSE)
      }, error = function(e) {
        showNotification(paste("qcc X-bar Phase I (unequal sizes) error:", e$message), type = "error")
        return(NULL)
      })
      q_r_p1 <- NULL # R chart not valid for unequal sizes
      q_xbar_p2 <- NULL
      if (length(phase2_mat$means) > 0) {
        q_xbar_p2 <- tryCatch({
          qcc(data = phase1_mat$means, sizes = phase1_mat$sizes, type = "xbar", newdata = phase2_mat$means, plot = FALSE)
        }, error = function(e) {
          showNotification(paste("qcc X-bar Phase II error:", e$message), type = "error")
          NULL
        })
      }
      res$q_xbar_p1 <- q_xbar_p1
      res$q_r_p1 <- q_r_p1
      res$q_xbar_p2 <- q_xbar_p2
    }
    
    # Identify out-of-control points
    ooc <- data.frame()
    if (!is.null(res$q_xbar_p2)) {
      # qcc stores statistics in the object; for monitoring newdata, the object has "violations" info accessible via $violations
      # But easier: compute which Phase II subgroup means are outside UCL/LCL from Phase I
      if (res$equal_size) {
        xbarbar <- res$q_xbar_p1$center
        sigma_xbar <- res$q_xbar_p1$std.dev
        UCL <- res$q_xbar_p1$limits[,2][1] # for xbar qcc stores matrix nrow = number of samples? but safe approach: compute from qcc$limits
        LCL <- res$q_xbar_p1$limits[,1][1]
        # Phase II subgroup means
        p2_groups <- res$phase2_idx
        p2_means <- sapply(groups[p2_groups], mean)
        ooc_idx <- which(p2_means > UCL | p2_means < LCL)
        if (length(ooc_idx) > 0) {
          ooc <- data.frame(group = p2_groups[ooc_idx], mean = p2_means[ooc_idx], UCL = UCL, LCL = LCL)
        }
      } else {
        # unequal sizes: use q_xbar_p1$limits (vector)
        limits <- res$q_xbar_p1$limits
        p2_groups <- res$phase2_idx
        p2_means <- sapply(groups[p2_groups], mean)
        UCL <- limits[2]; LCL <- limits[1]
        ooc_idx <- which(p2_means > UCL | p2_means < LCL)
        if (length(ooc_idx) > 0) {
          ooc <- data.frame(group = p2_groups[ooc_idx], mean = p2_means[ooc_idx], UCL = UCL, LCL = LCL)
        }
      }
    }
    res$ooc <- ooc
    res
  }, ignoreNULL = FALSE)
  
  # Render plots
  output$xbar_phase1_plot <- renderPlot({
    res <- results()
    req(res$q_xbar_p1)
    plot(res$q_xbar_p1, title = "X-bar Chart (Phase I)")
  })
  output$r_phase1_plot <- renderPlot({
    res <- results()
    if (res$equal_size && !is.null(res$q_r_p1)) {
      plot(res$q_r_p1, title = "R Chart (Phase I)")
    } else {
      plot.new()
      text(0.5, 0.5, "R chart not available (unequal subgroup sizes)", cex = 1.2)
    }
  })
  output$xbar_phase2_plot <- renderPlot({
    res <- results()
    if (!is.null(res$q_xbar_p2)) {
      plot(res$q_xbar_p2, title = "X-bar Chart (Phase I limits applied to Phase II)")
    } else {
      plot.new()
      text(0.5, 0.5, "No Phase II data to display", cex = 1.2)
    }
  })
  output$r_phase2_plot <- renderPlot({
    res <- results()
    if (res$equal_size && !is.null(res$q_r_p2)) {
      plot(res$q_r_p2, title = "R Chart (Phase I limits applied to Phase II)")
    } else {
      plot.new()
      text(0.5, 0.5, "No Phase II R-chart available", cex = 1.2)
    }
  })
  
  output$phase1_stats <- renderPrint({
    res <- results()
    req(res$q_xbar_p1)
    cat("Phase I X-bar summary:\n")
    print(res$q_xbar_p1$summary)
    if (res$equal_size && !is.null(res$q_r_p1)) {
      cat("\nPhase I R summary:\n")
      print(res$q_r_p1$summary)
    }
  })
  output$phase2_stats <- renderPrint({
    res <- results()
    if (!is.null(res$q_xbar_p2)) {
      cat("Phase II monitoring summary (applied Phase I limits):\n")
      # qcc object has statistics for newdata in $violations maybe; print summary
      print(res$q_xbar_p2$summary)
    } else {
      cat("No Phase II data or monitoring not performed.")
    }
  })
  
  output$ooc_tbl <- renderDT({
    res <- results()
    if (nrow(res$ooc) == 0) {
      datatable(data.frame(message = "No out-of-control Phase II subgroup means found (or no Phase II data)."), options = list(dom = 't'))
    } else {
      datatable(res$ooc, options = list(pageLength = 10))
    }
  })
}

shinyApp(ui, server)

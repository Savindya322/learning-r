#not showing phase1 charts others are ok

library(shiny)
library(qcc)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Universal QA Control Charts: Phase I & II"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Phase I (Upload)"),
      fileInput("file1", "Upload any QA CSV file", accept = ".csv"),
      helpText("Note: The app assumes Column 1 is a label and all other columns are measurements."),
      
      # Added: Displays how many samples were detected per group
      uiOutput("subgroup_info"),
      
      checkboxGroupInput("charts", "Select Charts:",
                         choices = c("X-bar Chart" = "xbar", "R Chart" = "R"),
                         selected = c("xbar", "R")),
      
      hr(),
      h4("2. Phase II (Monitoring)"),
      textAreaInput("phase2_input", "Enter Phase II values", 
                    placeholder = "Paste values here (comma or space separated)...", 
                    rows = 4),
      
      actionButton("update", "Update Phase II", class = "btn-primary w-100"),
      br(), br(),
      uiOutput("p2_status")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Control Charts", 
                 uiOutput("plots_ui")),
        tabPanel("Data Preview", 
                 tableOutput("preview"))
      )
    )
  )
)

server <- function(input, output) {
  
  # 1. Reactive for Phase I Data (Universal Detection)
  phase1_mat <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    
    # Check if file has at least 2 columns
    if(ncol(df) < 2) {
      showNotification("CSV must have at least 2 columns (1 Label + Samples)", type = "error")
      return(NULL)
    }
    
    # UNIVERSAL CHANGE: Drop column 1, take everything else as the matrix
    mat <- as.matrix(df[, -1])
    
    # Optional: Ensure data is numeric to prevent qcc errors
    if(!is.numeric(mat)) {
      showNotification("Non-numeric data detected in sample columns!", type = "error")
      return(NULL)
    }
    return(mat)
  })
  
  # Display the detected subgroup size to the user
  output$subgroup_info <- renderUI({
    req(phase1_mat())
    n <- ncol(phase1_mat())
    helpText(strong(paste("Detected subgroup size (n):", n)))
  })
  
  # 2. Reactive for Phase II Data (Adapts to any 'n')
  phase2_mat <- eventReactive(input$update, {
    p1 <- phase1_mat()
    req(p1)
    
    txt <- input$phase2_input
    n <- ncol(p1) # Detected 'n' from uploaded file
    
    if (nzchar(trimws(txt))) {
      # Robust splitting: handles spaces, commas, tabs, and new lines
      nums <- as.numeric(unlist(strsplit(txt, "[^0-9.-]+")))
      nums <- nums[!is.na(nums)]
      
      if (length(nums) >= n) {
        rows <- floor(length(nums) / n)
        # Reshape into a matrix with the correct number of columns
        return(matrix(nums[1:(rows * n)], ncol = n, byrow = TRUE))
      } else {
        showNotification(paste("Need at least", n, "values for one subgroup."), type = "warning")
      }
    }
    return(NULL)
  }, ignoreNULL = FALSE)
  
  # Status for Phase II
  output$p2_status <- renderUI({
    p2 <- phase2_mat()
    if (is.null(p2)) {
      span("Phase I active.", style="color: gray;")
    } else {
      strong(paste("Monitoring:", nrow(p2), "new subgroup(s)"), style="color: green;")
    }
  })
  
  # 3. Dynamic UI Layout (Vertical Stacking)
  output$plots_ui <- renderUI({
    req(input$file1)
    selected <- input$charts
    
    plot_output_list <- list()
    if ("xbar" %in% selected) {
      plot_output_list[[length(plot_output_list) + 1]] <- plotOutput("xbarPlot", height = "400px")
      plot_output_list[[length(plot_output_list) + 1]] <- br()
    }
    if ("R" %in% selected) {
      plot_output_list[[length(plot_output_list) + 1]] <- plotOutput("rPlot", height = "400px")
    }
    do.call(tagList, plot_output_list)
  })
  
  # 4. Renderers
  output$xbarPlot <- renderPlot({
    p1 <- phase1_mat()
    p2 <- phase2_mat()
    req(p1)
    
    if (is.null(p2)) {
      qcc(p1, type = "xbar", title = "X-bar Chart: Phase I")
    } else {
      qcc(p1, type = "xbar", newdata = p2, title = "X-bar Chart: Phase I & II")
    }
  })
  
  output$rPlot <- renderPlot({
    p1 <- phase1_mat()
    p2 <- phase2_mat()
    req(p1)
    
    if (is.null(p2)) {
      qcc(p1, type = "R", title = "R Chart: Phase I")
    } else {
      qcc(p1, type = "R", newdata = p2, title = "R Chart: Phase I & II")
    }
  })
  
  output$preview <- renderTable({
    req(input$file1)
    head(read.csv(input$file1$datapath), 10)
  })
}

shinyApp(ui, server)

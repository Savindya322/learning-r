library(shiny)
library(qcc)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    base_font = font_google("Inter")
  ),
  
  # Custom CSS to ensure plots take full width and reduce padding
  tags$head(
    tags$style(HTML("
      .shiny-plot-output { width: 100% !important; }
      .container-fluid { max-width: 100%; }
      .sidebar { margin-bottom: 20px; }
    "))
  ),
  
  div(
    class = "bg-primary text-white p-3 mb-3 rounded shadow-sm",
    h2(paste0("Control Charts: ", "X\u0305", " & R Charts"), class = "m-0")
  ),
  
  # Set widths: sidebar (3) + main (9) = 12 columns
  sidebarLayout(
    sidebarPanel(
      width = 3, # Reduced width for the menu bar
      fileInput("file1", "Upload Dataset (CSV)", accept = ".csv", width = "100%"),
      
      numericInput("p1_limit", "Number of Subgroups for Phase I:", 
                   value = 25, min = 2, step = 1, width = "100%"),
      helpText("Phase I data establishes control limits."),
      
      uiOutput("subgroup_info"),
      
      radioButtons(
        "chart_selection", "Select Chart Type:",
        choices = c("X\u0305 Chart" = "xbar", "R Chart" = "R"),
        selected = "xbar"
      ),
      
      hr(),
      h4("Additional Phase II Data"),
      textAreaInput(
        "phase2_input",
        "Enter values (comma/space separated)",
        placeholder = "e.g., 10.1, 10.2, 10.5...",
        rows = 3,
        width = "100%"
      ),
      actionButton(
        "update", "Update Phase II",
        class = "btn-primary w-100"
      ),
      br(), br(),
      uiOutput("p2_status"),
      
      hr(),
      actionButton(
        "reset_all", "Reset & Clear All",
        class = "btn-danger w-100",
        icon = icon("rotate")
      )
    ),
    
    mainPanel(
      width = 9, # Increased width for the charts
      tabsetPanel(
        tabPanel("Control Charts", 
                 uiOutput("plots_ui")),
        tabPanel("Data Preview", 
                 div(class = "mt-3", tableOutput("preview")))
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reset_all, {
    session$reload()
  })
  
  full_file_mat <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    if (ncol(df) < 2) return(NULL)
    mat <- as.matrix(df[, -1])
    if (!all(apply(mat, 2, is.numeric))) {
      showNotification("Non-numeric data detected!", type = "error")
      return(NULL)
    }
    mat
  })
  
  phase1_mat <- reactive({
    mat <- full_file_mat()
    req(mat, input$p1_limit)
    limit <- min(nrow(mat), input$p1_limit)
    mat[1:limit, , drop = FALSE]
  })
  
  manual_p2_store <- reactiveVal(NULL)
  
  observeEvent(input$update, {
    req(full_file_mat())
    txt <- input$phase2_input
    if (!nzchar(trimws(txt))) {
      manual_p2_store(NULL)
      return()
    }
    
    n <- ncol(full_file_mat())
    nums <- as.numeric(unlist(strsplit(txt, "[^0-9.-]+")))
    nums <- nums[!is.na(nums)]
    
    if (length(nums) < n) {
      showNotification("Not enough values for a full subgroup", type = "warning")
      return()
    }
    
    rows <- floor(length(nums) / n)
    manual_p2_store(matrix(nums[1:(rows * n)], ncol = n, byrow = TRUE))
  })
  
  combined_p2_mat <- reactive({
    req(full_file_mat(), input$p1_limit)
    
    file_rows <- nrow(full_file_mat())
    file_p2 <- if(file_rows > input$p1_limit) {
      full_file_mat()[(input$p1_limit + 1):file_rows, , drop = FALSE]
    } else {
      NULL
    }
    
    if (is.null(manual_p2_store())) {
      return(file_p2)
    } else {
      if (is.null(file_p2)) return(manual_p2_store())
      return(rbind(file_p2, manual_p2_store()))
    }
  })
  
  output$plots_ui <- renderUI({
    if (is.null(input$file1)) {
      return(div(class = "mt-5 text-center text-muted", 
                 h4("Please upload a CSV file to generate charts.")))
    }
    
    req(input$chart_selection)
    p1_title <- paste("Phase I: Establishing Limits (First", input$p1_limit, "Subgroups)")
    
    # Graphs now use 100% width via CSS and specified height
    if (input$chart_selection == "xbar") {
      tagList(
        h5(p1_title, class = "mt-3"),
        plotOutput("xbarP1", height = "400px", width = "100%"),
        hr(),
        h5("Phase II: Monitoring Process", class = "mt-3"),
        plotOutput("xbarP2", height = "400px", width = "100%")
      )
    } else {
      tagList(
        h5(p1_title, class = "mt-3"),
        plotOutput("rP1", height = "400px", width = "100%"),
        hr(),
        h5("Phase II: Monitoring Process", class = "mt-3"),
        plotOutput("rP2", height = "400px", width = "100%")
      )
    }
  })
  
  # --- Rendering Logic ---
  output$xbarP1 <- renderPlot({
    req(phase1_mat())
    qcc(phase1_mat(), type = "xbar", title = "")
    title(main = expression(bar(X) ~ "Chart: Phase I"))
  })
  
  output$xbarP2 <- renderPlot({
    req(phase1_mat())
    qcc(data = phase1_mat(), 
        type = "xbar", 
        newdata = combined_p2_mat(), 
        title = "")
    title(main = expression(bar(X) ~ "Chart: Phase II"))
  })
  
  output$rP1 <- renderPlot({
    req(phase1_mat())
    qcc(phase1_mat(), type = "R", title = "R Chart: Phase I")
  })
  
  output$rP2 <- renderPlot({
    req(phase1_mat())
    qcc(data = phase1_mat(), 
        type = "R", 
        newdata = combined_p2_mat(), 
        title = "R Chart: Phase II")
  })
  
  output$subgroup_info <- renderUI({
    req(phase1_mat())
    tagList(
      helpText(strong("Subgroup size (n):", ncol(phase1_mat()))),
      helpText(strong("Current Phase I size:", nrow(phase1_mat()), "subgroups"))
    )
  })
  
  output$p2_status <- renderUI({
    req(full_file_mat())
    count <- if(is.null(combined_p2_mat())) 0 else nrow(combined_p2_mat())
    strong(paste("Total Phase II subgroups:", count), style = "color: #51cf66;")
  })
  
  output$preview <- renderTable({
    req(input$file1)
    head(read.csv(input$file1$datapath), 10)
  }, width = "100%")
}

shinyApp(ui, server)

library(shiny)
library(qcc)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",   # 🌙 Dark mode
    base_font = font_google("Inter")
  ),
  
  titlePanel("Universal QA Control Charts: Phase I & II"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Phase I (Upload)"),
      fileInput("file1", "Upload any QA CSV file", accept = ".csv"),
      helpText("Column 1 = label, remaining columns = subgroup measurements"),
      
      uiOutput("subgroup_info"),
      
      checkboxGroupInput(
        "charts", "Select Charts:",
        choices = c("X-bar Chart" = "xbar", "R Chart" = "R"),
        selected = c("xbar", "R")
      ),
      
      hr(),
      h4("2. Phase II (Monitoring)"),
      textAreaInput(
        "phase2_input",
        "Enter Phase II values",
        placeholder = "Paste values here (comma / space separated)",
        rows = 4
      ),
      actionButton(
        "update", "Update Phase II",
        class = "btn-primary w-100"
      ),
      br(), br(),
      uiOutput("p2_status")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Control Charts", uiOutput("plots_ui")),
        tabPanel("Data Preview", tableOutput("preview"))
      )
    )
  )
)

server <- function(input, output) {
  
  # -------------------------
  # Phase I data
  # -------------------------
  phase1_mat <- reactive({
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
  
  output$subgroup_info <- renderUI({
    req(phase1_mat())
    helpText(
      strong(
        paste("Detected subgroup size (n):", ncol(phase1_mat())),
        style = "color: #f8f9fa;"
      )
    )
  })
  
  # -------------------------
  # Phase II storage
  # -------------------------
  phase2_store <- reactiveVal(NULL)
  
  observeEvent(input$update, {
    req(phase1_mat())
    
    txt <- input$phase2_input
    if (!nzchar(trimws(txt))) {
      phase2_store(NULL)
      return()
    }
    
    n <- ncol(phase1_mat())
    nums <- as.numeric(unlist(strsplit(txt, "[^0-9.-]+")))
    nums <- nums[!is.na(nums)]
    
    if (length(nums) < n) {
      showNotification("Not enough Phase II values", type = "warning")
      phase2_store(NULL)
      return()
    }
    
    rows <- floor(length(nums) / n)
    phase2_store(matrix(nums[1:(rows * n)], ncol = n, byrow = TRUE))
  })
  
  output$p2_status <- renderUI({
    if (is.null(phase2_store())) {
      span("Phase I active.", style = "color: #adb5bd;")
    } else {
      strong(
        paste("Monitoring:", nrow(phase2_store()), "new subgroup(s)"),
        style = "color: #51cf66;"
      )
    }
  })
  
  # -------------------------
  # Dynamic UI
  # -------------------------
  output$plots_ui <- renderUI({
    req(phase1_mat())
    
    plots <- list()
    
    if ("xbar" %in% input$charts) {
      plots[[length(plots) + 1]] <- plotOutput("xbarPlot", height = "400px")
      plots[[length(plots) + 1]] <- br()
    }
    
    if ("R" %in% input$charts) {
      plots[[length(plots) + 1]] <- plotOutput("rPlot", height = "400px")
    }
    
    do.call(tagList, plots)
  })
  
  # -------------------------
  # X-bar Chart
  # -------------------------
  output$xbarPlot <- renderPlot({
    p1 <- phase1_mat()
    p2 <- phase2_store()
    req(p1)
    
    if (is.null(p2)) {
      print(qcc(p1, type = "xbar",
                title = "X-bar Chart: Phase I"))
    } else {
      print(qcc(p1, type = "xbar",
                newdata = p2,
                title = "X-bar Chart: Phase I & II"))
    }
  })
  
  # -------------------------
  # R Chart
  # -------------------------
  output$rPlot <- renderPlot({
    p1 <- phase1_mat()
    p2 <- phase2_store()
    req(p1)
    
    if (is.null(p2)) {
      print(qcc(p1, type = "R",
                title = "R Chart: Phase I"))
    } else {
      print(qcc(p1, type = "R",
                newdata = p2,
                title = "R Chart: Phase I & II"))
    }
  })
  
  output$preview <- renderTable({
    req(input$file1)
    head(read.csv(input$file1$datapath), 10)
  })
}

shinyApp(ui, server)

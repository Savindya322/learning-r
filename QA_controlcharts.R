library(shiny)
library(qcc)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    base_font = font_google("Inter")
  ),
  
  titlePanel("QA Control Charts: Phase I & II"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Upload"),
      fileInput("file1", "Upload CSV file", accept = ".csv"),
      helpText("Note: Rows 1-25 will define Phase I Control Limits."),
      
      uiOutput("subgroup_info"),
      
      radioButtons(
        "chart_selection", "Select Chart Type:",
        choices = c("X-bar Chart" = "xbar", "R Chart" = "R"),
        selected = "xbar"
      ),
      
      hr(),
      h4("2. Additional Phase II Data"),
      helpText("Add values manually to append to the file data:"),
      textAreaInput(
        "phase2_input",
        "Enter values (comma/space separated)",
        placeholder = "e.g., 10.1, 10.2, 10.5...",
        rows = 3
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
        tabPanel("Control Charts", 
                 uiOutput("plots_ui")),
        tabPanel("Data Preview", 
                 tableOutput("preview"))
      )
    )
  )
)

server <- function(input, output) {
  
  # --- Data Processing ---
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
    req(mat)
    limit <- min(nrow(mat), 25)
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
    req(full_file_mat())
    if (is.null(manual_p2_store())) {
      return(full_file_mat())
    } else {
      return(rbind(full_file_mat(), manual_p2_store()))
    }
  })
  
  # --- Dynamic Plot UI ---
  output$plots_ui <- renderUI({
    req(input$chart_selection)
    
    if (input$chart_selection == "xbar") {
      tagList(
        h5("Phase I: Establishing Control Limits (First 25 Subgroups)", class = "mt-3"),
        plotOutput("xbarP1", height = "350px"),
        hr(),
        h5("Phase II", class = "mt-3"),
        plotOutput("xbarP2", height = "350px")
      )
    } else {
      tagList(
        h5("Phase I: Establishing Range Limits (First 25 Subgroups)", class = "mt-3"),
        plotOutput("rP1", height = "350px"),
        hr(),
        h5("Phase II", class = "mt-3"),
        plotOutput("rP2", height = "350px")
      )
    }
  })
  
  # --- Rendering Logic ---
  
  # X-Bar Plots
  output$xbarP1 <- renderPlot({
    req(phase1_mat())
    qcc(phase1_mat(), type = "xbar", title = "X-bar Chart: Phase I")
  })
  
  output$xbarP2 <- renderPlot({
    req(phase1_mat(), combined_p2_mat())
    qcc(data = phase1_mat(), 
        type = "xbar", 
        newdata = combined_p2_mat(), 
        title = "X-bar Chart: Phase II")
  })
  
  # R Plots
  output$rP1 <- renderPlot({
    req(phase1_mat())
    qcc(phase1_mat(), type = "R", title = "R Chart: Phase I")
  })
  
  output$rP2 <- renderPlot({
    req(phase1_mat(), combined_p2_mat())
    qcc(data = phase1_mat(), 
        type = "R", 
        newdata = combined_p2_mat(), 
        title = "R Chart: Phase II")
  })
  
  # --- Side UI Info ---
  output$subgroup_info <- renderUI({
    req(phase1_mat())
    tagList(
      helpText(strong("Subgroup size (n):", ncol(phase1_mat()))),
      helpText(strong("Phase I size:", nrow(phase1_mat()), "subgroups"))
    )
  })
  
  output$p2_status <- renderUI({
    req(combined_p2_mat())
    strong(paste("Total Phase II points:", nrow(combined_p2_mat())), style = "color: #51cf66;")
  })
  
  output$preview <- renderTable({
    req(input$file1)
    head(read.csv(input$file1$datapath), 10)
  })
}

shinyApp(ui, server)
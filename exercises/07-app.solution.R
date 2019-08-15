library(readr)
library(ggplot2)
library(rlang)
library(shiny)
library(shinymeta)
library(shinyAce)

setup_code <- quote({
  library(readr)
  library(ggplot2)
  library(rlang)
  
  "# Identify the file we're going to load, relative to project root"
  filepath <- "safety_data.csv"
  
  "# Load CSV data"
  safety <- read_csv(filepath, comment = "#")
})
eval(setup_code)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("column_ui")
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("histogram")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      uiOutput("column2_ui")
    ),
    mainPanel(
      plotOutput("scatter"),
      h3(
        strong("Correlation: "),
        textOutput("cor", inline = TRUE)
      )
    )
  ),
  fixedPanel(bottom = 12, right = 12, style = "z-index: 1000;",
    downloadButton("downloadReport", "Download report")
  )
)

server <- function(input, output, session) {
  column <- metaReactive2({
    req(input$column)
    metaExpr(..(input$column))
  })
  
  column2 <- metaReactive2({
    req(input$column2)
    metaExpr(..(input$column2))
  })
  
  output$column_ui <- renderUI({
    selectInput("column", "Column to analyze",
      choices = names(safety)[c(-1, -2)]
    )
  })
  
  output$column2_ui <- renderUI({
    selectInput("column2", "Second column",
      choices = names(safety)[c(-1, -2)]
    )
  })
  
  output$summary <- metaRender(renderPrint, {
    # Print a basic summary
    summary(safety[[..(column())]])
  })
  
  output$histogram <- metaRender(renderPlot, {
    # Plot a histogram of the column in question
    ggplot(safety, aes(!!sym(..(column())), fill = Class)) +
      geom_histogram(bins = 30) +
      facet_wrap(~Class) +
      ggtitle(..(column()))
  })
  
  output$scatter <- metaRender(renderPlot, {
    # Plot a scatter plot of column vs. column2
    ggplot(safety, aes(!!sym(..(column())), !!sym(..(column2())), color = Class)) +
      geom_point(size = 3, alpha = 0.5) +
      geom_smooth(se = FALSE) +
      ggtitle(paste(..(column()), "/", ..(column2())))
  })
  
  output$cor <- metaRender(renderText, {
    # Calculate correlation
    cor(safety[[..(column())]], safety[[..(column2())]], use = "complete.obs")
  })

  output$downloadReport <- downloadHandler("report.zip",
    function(file) {
      code <- expandChain(
        metaExpr(setup_code, quoted = TRUE),
        output$summary(),
        output$histogram(),
        output$scatter(),
        output$cor()
      )
      
      buildRmdBundle("07-report.Rmd", file, vars = list(
        code = code,
        column = column(),
        filepath = filepath
      ), include_files = filepath)
    }
  )
}

shinyApp(ui, server)

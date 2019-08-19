library(readr)
library(ggplot2)
library(rlang)
library(shiny)
library(shinymeta)

# Identify the file we're going to load, relative to project root
filepath <- "safety_data.csv"

# Load CSV data
safety <- read_csv(filepath, comment = "#")


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
  )
)

server <- function(input, output, session) {
  column <- metaReactive({
    req(input$column)
    input$column
  })
  
  column2 <- metaReactive({
    req(input$column2)
    input$column2
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
    summary(safety[[column()]])
  })
  
  output$histogram <- metaRender(renderPlot, {
    # Plot a histogram of the column in question
    ggplot(safety, aes(!!sym(column()), fill = Class)) +
      geom_histogram(bins = 30) +
      facet_wrap(~Class) +
      ggtitle(column())
  })
  
  output$scatter <- metaRender(renderPlot, {
    # Plot a scatter plot of column vs. column2
    ggplot(safety, aes(!!sym(column()), !!sym(column2()), color = Class)) +
      geom_point(size = 3, alpha = 0.5) +
      geom_smooth(se = FALSE) +
      ggtitle(paste(column(), "/", column2()))
  })
  
  output$cor <- metaRender(renderText, {
    # Calculate correlation
    cor(safety[[column()]], safety[[column2()]], use = "complete.obs")
  })
  
  observe({
    print(expandChain(
      # Sorry about the duplicated code, metaAction is coming soon!
      quote({
        library(readr)
        library(ggplot2)
        library(rlang)
        
        # Identify the file we're going to load, relative to project root
        filepath <- "safety_data.csv"
        
        # Load CSV data
        safety <- read_csv(filepath, comment = "#")
      }),
      output$summary(),
      output$histogram(),
      output$scatter(),
      output$cor()
    ))
  })
}

shinyApp(ui, server)

library(shiny)
library(data.table)
dailyData <- fread("C:/Users/minunno/Documents/vdlData/processedData/dailyData.csv")
# dailyData <- dailyData[id %in% unique(dailyData$id)[1:2]]
dailyData$dates <- as.Date(dailyData$dates)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a sensor:",
                  choices = unique(dailyData$id)),
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "variable",
                  label = "Choose variable:",
                  choices = names(dailyData)[c(2,3,5)])
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  # Return the requested dataset ----
  # datasetInput <- reactive({
  #   dailyData[id==input$dataset]
  # })
  output$distPlot <- renderPlot({

    x    <- dailyData[id==input$dataset,dates]
    # print(input$variable)
    y    <- dailyData[id==input$dataset,input$variable,with=FALSE]
    # print(length(y))
    # print(length(x))
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    plot(x,y[[1]],pch=20,ylab =input$variable,xlab="day" ,main=paste0("sensor ID: ",input$dataset))
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
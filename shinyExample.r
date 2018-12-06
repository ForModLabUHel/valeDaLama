library(shiny)
library(data.table)
library(ggplot2)
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
      selectInput(inputId = "variable",
                  label = "Choose variable:",
                  choices = names(dailyData)[c(2,3,5)]),
      checkboxGroupInput(inputId = "dataset", label = "Choose a sensor:", 
                         choices=unique(dailyData$id),
                         selected = NULL, inline = FALSE)
            # Input: Selector for choosing dataset ----
      # selectInput(inputId = "dataset",
      #             label = "Choose a sensor:",
      #             choices = unique(dailyData$id)),
      # # Input: Selector for choosing dataset ----
      
      
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

    # x    <- dailyData[id==input$dataset,dates]
    # print(input$variable)
    subData    <- dailyData[id %in% input$dataset,c("id","dates",input$variable),with=FALSE]
    # print(length(y))
    # print(length(x))
    dailyData$id <- factor(dailyData$id)
   
    ggplot(data=subData, 
           aes_string(x = "dates", y = input$variable,group="id",color="id", shape="id")) +
      scale_shape_manual(values=1:nlevels(dailyData$id)) +
      # labs(title = "Dead Wood")+
      xlab("date") +
      ylab(input$variable) +
      # xlim(2012,2100) + 
      # ylim(0,5500) + 
      # geom_line()
      geom_point()
    
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    # plot(x,y[[1]],pch=20,ylab =input$variable,xlab="day" ,main=paste0("sensor ID: ",input$dataset))
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
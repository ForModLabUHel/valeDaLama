library(lubridate);library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
# allData <- fread("/Users/walterludwick/Documents/data_vdl/allData.csv") #path for wl
allData <- fread("C:/Users/minunno/Documents/vdlData/processedData/allData.csv") #path for fm
load("data/consistData.rdata") ##read data for which fp_id and serial number are consistent
allData <- allData[serial_number %in% consistData] ##select data for which fp_id and serial number are consistent
allData$dates <- as.POSIXct(allData$dates)
allData$dSM <- NA

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "Xaxis",
                  label = "Choose variable for x axis:",
                  choices = c("light", "air_temperature_celsius",
                              "soil_moisture_percent","dSM","dates")),
      selectInput(inputId = "Yaxis",
                  label = "Choose variable for y axis:",
                  choices = c("light", "air_temperature_celsius",
                              "soil_moisture_percent","dSM","dates")),
      # Input: Selector for choosing timestep ----
      selectInput(inputId = "timestep",
                  label = "Choose a timestep:",
                  choices = c("15 min","30 min","1 h","3 h","6 h","12 h","1 d")),
      
    selectInput(inputId = "startdate",
                label = "Choose starting date:",
                choices = unique(ceiling_date(allData$dates, "day"))),
    selectInput(inputId = "enddate",
                label = "Choose end date:",
                choices = unique(ceiling_date(allData$dates, "day")),
                selected = max(unique(ceiling_date(allData$dates, "day")))),
    
    checkboxGroupInput(inputId = "dataset", label = "Choose a sensor:", 
                         choices=unique(allData$longName),
                         selected = NULL, inline = FALSE)
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
  #   allData[id==input$dataset]
  # })
  output$distPlot <- renderPlot({

    # x    <- allData[id==input$dataset,dates]
    # print(input$variable)
    subData <- allData[dates %between% c(input$startdate, input$enddate)]
    subData    <- subData[longName %in% input$dataset]
    subData[,dates:=cut(subData$dates, breaks=input$timestep)]
    subData <- subData[, lapply(.SD, mean, na.rm=TRUE), by=list(longName,dates), 
                       .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ] 
    subData <- subData %>% group_by(longName) %>% 
      mutate(dSM = order_by(dates, soil_moisture_percent - lag(soil_moisture_percent)))
    
    
    subData$longName <- factor(subData$longName)
    subData$dates <- as.Date(subData$dates)
    ggplot(data=subData,
           aes_string(x = input$Xaxis, y = input$Yaxis,group="longName",color="longName", shape="longName")) +
      scale_shape_manual(values=1:nlevels(subData$longName)) +
      xlab(input$Xaxis) +
      ylab(input$Yaxis) +
      # scale_x_date(labels = date_format("%m-%Y"))+
      # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      # scale_x_discrete(breaks = round(seq(as.Date(input$startdate), as.Date(input$enddate),
      #                                       length.out = 5),1)) +
      # geom_line()
      geom_point()

        # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    # plot(x,y[[1]],pch=20,ylab =input$axis,xlab="day" ,main=paste0("sensor ID: ",input$dataset))
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
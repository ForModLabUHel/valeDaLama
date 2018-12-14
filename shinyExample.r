Sys.setenv(RSTUDIO_PANDOC="/Users/walterludwick/anaconda3/bin/pandoc")

library(lubridate);library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)

allData <- fread("/Users/walterludwick/Documents/data_vdl/allData.csv") #path for wl
# allData <- fread("C:/Users/minunno/Documents/vdlData/processedData/allData.csv") #path for fm
# allData <- fread("/Users/walterludwick/Documents/data_vdl/allData.csv") #path for wl
# allData <- fread("C:/Users/minunno/Documents/vdlData/processedData/allData.csv") #path for fm

#### read file from DropBox
dir.create( "dropbox" )
download.file("https://www.dropbox.com/s/ngaexvxlazshb0j/allData.csv?dl=1", 
              "./dropbox/allData.csv" )
allData <- fread( "./dropbox/allData.csv" )
unlink( "dropbox", recursive = TRUE )
destfile <- "/Users/walterludwick/Documents/data_vdl/allData.csv" #path for wl
# destfile <- "C:/Users/minunno/Documents/vdlData/processedData/allData.csv" #path for fm

if(file.exists(destfile)){
  allData <-  fread(destfile)
}else{
  #### read file from DropBox
  allData <- fread( "https://www.dropbox.com/s/ngaexvxlazshb0j/allData.csv?dl=1")
}

>>>>>>> 40d965c71f776500477d65d507294e906a70d615

<<<<<<< HEAD
>>>>>>> 5a6608e8d325f9caa78b3e79ba1e42fba73d16d5
=======
>>>>>>> 5a6608e8d325f9caa78b3e79ba1e42fba73d16d5
# load("data/consistData.rdata") ##read data for which fp_id and serial number are consistent
# allData <- allData[serial_number %in% consistData] ##select data for which fp_id and serial number are consistent
allData$dates <- as.POSIXct(allData$dates)
allData$dSM <- NA
selTab <- fread("data/selTab.csv")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Vale da lama GROW sensors"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "Xaxis",
                  label = "Choose variable for x axis:",
                  choices = c("light", "air_temperature_celsius",
                              "soil_moisture_percent","dSM","dates"),
                  selected = "dates"),
      selectInput(inputId = "Yaxis",
                  label = "Choose variable for y axis:",
                  choices = c("light", "air_temperature_celsius",
                              "soil_moisture_percent","dSM","dates"),
                  selected = "soil_moisture_percent"),
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
    selectizeInput(inputId = "selByClass",
                   label = "select sensors by class", 
                   choices = unique(selTab$CLASS), multiple = TRUE),
    selectizeInput(inputId = "selByVdl",
                   label = "select sensors by vdl_id", 
                   choices = unique(selTab$VDL_ID), multiple = TRUE),
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
server <- function(input, output,session) {
  
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
  observe({
    siteVdl <- selTab$longName[selTab$VDL_ID %in% input$selByVdl]
    siteClass <- selTab$longName[selTab$CLASS %in% input$selByClass]
    # siteSel <- input$dataset
    if(is.null(input$selByVdl) & is.null(input$selByClass)){
      sites <- unique(allData$longName)
    }else{
      sites <- unique(c(siteVdl,siteClass))
    }


    # Can use character(0) to remove all choices
    # if (is.null(sites))
    #   sites <- unique(allData$longName)
    
    updateCheckboxGroupInput(session, "dataset",
                             label = "Choose a sensor:",
                             choices = sites)
  })
  output$distPlot <- renderPlot({

    # x    <- allData[id==input$dataset,dates]
    # print(input$variable)

      sites <- input$dataset
      subData <- allData[dates %between% c(input$startdate, input$enddate)]
      subData    <- subData[longName %in% sites]
      if(nrow(subData)>1) subData[,dates:=cut(subData$dates, breaks=input$timestep)]
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
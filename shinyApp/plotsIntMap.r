library(knitr)
library(lubridate);library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(curl)
library(gridExtra)
library(leaflet);library(leaflet.extras)

load("~/Dropbox/sensing_mission/data_vdl/processedData/allData.rdata")
# load("C:/Users/minunno/Documents/data_vdl/processedData/allData.rdata")
# load("allData.rdata")
selTab[,VDL_ZONE:=substr(vdlName,1,2)]

allData$dSM <- NA


## @knitr plots
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # # App title ----
  # titlePanel("Vale da lama GROW sensors"),
  
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
                  choices = c("15 min","30 min","1 h","3 h","6 h","12 h","1 d"),
                  selected = "1 d"),
      
      selectInput(inputId = "startdate",
                  label = "Choose starting date:",
                  choices = startDates),
      selectInput(inputId = "enddate",
                  label = "Choose end date:",
                  choices = endDates,
                  selected = maxEndDates),
      selectInput(inputId = "lastMeas",
                  label = "Choose sensors according to last measurements date:",
                  choices = lastSoilMeass,
                  selected = minLastSoilMeas),
      selectizeInput(inputId = "selByClass",
                     label = "select sensors by class", 
                     choices = sort(unique(selTab$VDL_CLASS)), multiple = TRUE),
      selectInput(inputId = "selSens",
                  label = "",
                  choices = c("or","and")),
      selectizeInput(inputId = "selByZone",
                     label = "select sensors by zone", 
                     choices = sort(unique(selTab$VDL_ZONE)), multiple = TRUE),
      selectizeInput(inputId = "selByVdl",
                     label = "select sensors by vdl_id", 
                     choices = sort(unique(selTab$VDL_ID)), multiple = TRUE),
      p(),
      actionButton("clearSel", "Clear sensors"),
      checkboxGroupInput(inputId = "dataset", label = "Choose a sensor:", 
                         choices=sort(unique(selTab$vdlName)),
                         selected = NULL, inline = FALSE)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      leafletOutput(outputId = "map")
    )
  )
)

lmap <- leaflet() %>%
  addTiles() %>%   
  # clearShapes() %>%
  fitBounds(-8.641081, 37.13735, -8.621029, 37.14682)  %>% 
  addCircleMarkers(lat = selTab$LAT, lng = selTab$LON, 
                   label=selTab$vdlName,radius = 3) #%>% 


# Define server logic required to draw a histogram ----
server <- function(input, output,session) {
  siteClick <- siteClickNew <- character(0)
  sitesSel <- character(0)
  subCoord <<-NULL
  data <- reactiveValues(clickedMarker=NULL)
  
  output$map <- renderLeaflet(lmap)
  
  observeEvent(input$clearSel, {
    data$clickedMarker <<- NULL
    subCoord <<-NULL
    siteClick <<- NULL
    siteClickNew <<-NULL
    if(input$timestep=="1 d"){
      subData <<- dailyData[last_soilMes >= input$lastMeas]
    }else{
      subData <<- allData[last_soilMes >= input$lastMeas]
    }
    sitesSel <<- NULL
    updateCheckboxGroupInput(session, "dataset",
                             label = "Choose sensors:",
                             choices = sort(sites),
                             selected = NULL)
    proxy <- leafletProxy('map')
    proxy %>% 
      addTiles() %>%   
      clearMarkers() %>%
      # clearShapes() %>%
      addCircleMarkers(lat = selTab$LAT, lng = selTab$LON, 
                       label=selTab$vdlName,radius = 3) %>%
      markerOptions(interactive = TRUE, clickable = TRUE,
                    draggable = FALSE)
    # updateSelectizeInput(session, 'selByVdl', selected = NULL, choices = unique(selTab$VDL_ID),server = TRUE)
    # updateSelectizeInput(session, 'selByClass', selected = NULL,choices = unique(selTab$CLASS), server = TRUE)
  })
  observeEvent(input$dataset, {
    
    data$clickedMarker <<- siteClick <<- input$dataset
    # print(siteClick)
    # print(siteClickNew)
    # print(input$map_marker_click$lng)
    proxy <- leafletProxy('map')
    if (length(input$dataset)>0){
      subCoord = selTab[which(selTab$vdlName %in% input$dataset),.(LAT,LON,vdlName)]
      proxy %>%
        addTiles() %>%
        clearMarkers() %>%
        # clearShapes() %>%
        addCircleMarkers(lat = selTab$LAT, lng = selTab$LON,
                         label=selTab$vdlName,radius = 3) %>%
        addCircleMarkers(lat = subCoord$LAT, lng = subCoord$LON,color = "red",
                         label=subCoord$vdlName,radius = 4) %>%
        markerOptions(interactive = TRUE, clickable = TRUE,
                      draggable = FALSE)
    }
  })
  
  # observeEvent(input$map_marker_click$lng, {
  #   print(siteClick)
  #   # print(siteClickNew)
  #   # print(input$map_marker_click$lng)
  # })
    
  observeEvent(input$map_marker_click,{
    # <- input$map_marker_click
    df <- selTab %>% filter(LON == input$map_marker_click$lng &
                              LAT == input$map_marker_click$lat)
    siteClickNew <- df$vdlName
    if(any(siteClickNew %in% siteClick)){
      data$clickedMarker <<- siteClick[!siteClick %in% siteClickNew]
    } else{
      data$clickedMarker <<- c(siteClick,siteClickNew)
    }
    if(length(data$clickedMarker)==0){
      proxy <- leafletProxy('map')
      proxy %>% 
        addTiles() %>%   
        clearMarkers() %>%
        # clearShapes() %>%
        addCircleMarkers(lat = selTab$LAT, lng = selTab$LON, 
                         label=selTab$vdlName,radius = 3) %>%
        markerOptions(interactive = TRUE, clickable = TRUE,
                      draggable = FALSE)
    } 
  }
  )
  
  observe({
    # sites <- input$dataset
    if(input$timestep=="1 d"){
      subData <<- dailyData[last_soilMes >= input$lastMeas]
    }else{
      subData <<- allData[last_soilMes >= input$lastMeas]
    }
    siteVdl <- selTab$vdlName[selTab$VDL_ID %in% input$selByVdl]
    siteClass <- selTab$vdlName[selTab$VDL_CLASS %in% input$selByClass]
    siteZone <- selTab$vdlName[selTab$VDL_ZONE %in% input$selByZone]
    # siteSel <- input$dataset
    if(is.null(input$selByVdl) & is.null(input$selByClass) & is.null(input$selByZone)){
      sites <- unique(allData$vdlName)
      siteX = F; sitesSel <<- NULL
    }else if(input$selSens == "and"){
      sites <- sitesSel <<- intersect(intersect(siteVdl,siteClass),siteZone)
      siteX = T
    }else{
      sites <- sitesSel <<- unique(c(siteVdl,siteClass,siteZone))
      siteX = T
    }
    sites <- intersect(sites,unique(subData$vdlName))
    siteClick <<- data$clickedMarker
    
      if(siteX){
        sites <- unique(c(sites,siteClick))
        sitesSel <<- data$clickedMarker <<- siteClick
      } else{
        sitesSel <<- data$clickedMarker <<- siteClick
      }
    # }
    nSites <<- length(sites)
    sites <<- sites
    updateCheckboxGroupInput(session, "dataset",
                             label = "Choose sensors:",
                             choices = sort(sites),
                             selected = sitesSel)
  })
  
  
  
  output$distPlot <- renderPlot({
    sites <- input$dataset
    plotData <- subData
    
    plotData <- plotData[dates %between% c(input$startdate, input$enddate)]
    plotData <- plotData[vdlName %in% sites]
    if(nrow(plotData)>1) plotData[,dates:=cut(plotData$dates, breaks=input$timestep)]
    plotData <- plotData[, lapply(.SD, mean, na.rm=TRUE), by=list(vdlName,dates),
                         .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ]
    plotData <- plotData %>% group_by(vdlName) %>%
      mutate(dSM = order_by(dates, soil_moisture_percent - lag(soil_moisture_percent)))
    
    
    # plotData$vdlName <- factor(plotData$vdlName)
    plotData$dates <- as.Date(plotData$dates)
    
    plot1 <- ggplot(data=plotData,
                    aes_string(x = input$Xaxis, y = input$Yaxis,group="vdlName",color="vdlName", shape="vdlName")) +
      scale_shape_manual(values=1:nlevels(plotData$vdlName)) +
      xlab(input$Xaxis) +
      ylab(input$Yaxis) +
      # scale_x_date(labels = date_format("%m-%Y"))+
      # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      # scale_x_discrete(breaks = round(seq(as.Date(input$startdate), as.Date(input$enddate),
      #                                       length.out = 5),1)) +
      # geom_line()
      geom_point() +
      labs(caption = paste(length(unique(plotData$vdlName)), "of",
                           nSites, "available sensors"))
    
    print(plot1)
  })  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server,options = list(height = 600,width=800))

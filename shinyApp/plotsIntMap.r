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

allData$dSM <- NA
selTab <- fread("~/Dropbox/sensing_mission/data_vdl/processedData/selTab.csv")
# selTab <- fread("C:/Users/minunno/Documents/data_vdl/processedData/selTab.csv")

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
                  choices = sort(unique(floor_date(allData$dates, "day")))),
      selectInput(inputId = "enddate",
                  label = "Choose end date:",
                  choices = sort(unique(ceiling_date(allData$dates, "day")),decreasing = T),
                  selected = max(unique(ceiling_date(allData$dates, "day")))),
      selectInput(inputId = "lastMeas",
                  label = "Choose sensors according to last measurements date:",
                  choices = sort(unique(floor_date(allData$last_soilMes, "day"))),
                  selected = min(unique(floor_date(allData$last_soilMes, "day")))),
      selectizeInput(inputId = "selByClass",
                     label = "select sensors by class", 
                     choices = unique(selTab$CLASS), multiple = TRUE),
      selectInput(inputId = "selSens",
                  label = "",
                  choices = c("or","and")),
      selectizeInput(inputId = "selByVdl",
                     label = "select sensors by vdl_id", 
                     choices = unique(selTab$VDL_ID), multiple = TRUE),
      p(),
      actionButton("clearSel", "Clear sensors"),
      checkboxGroupInput(inputId = "dataset", label = "Choose a sensor:", 
                         choices=unique(allData$vdlName),
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
  siteClick <- character(0)
  sitesSel <- character(0)
  SiteX <- NULL
  
  output$map <- renderLeaflet(lmap)
  
  observeEvent(input$clearSel, {
    siteClick <<- character(0)
    updateCheckboxGroupInput(session, "dataset",
                             label = "Choose sensors:",
                             choices = sort(selTab$vdlName),
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
  })
  
  observeEvent(input$dataset, {
    siteClick <<- input$dataset
  })
  
  observeEvent(input$dataset, {
    proxy <- leafletProxy('map')
    if (length(input$dataset)>0){ 
      subCoord <- selTab[which(selTab$vdlName %in% input$dataset),.(LAT,LON,vdlName)]
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
  
  observe({
    # sites <- input$dataset
    subData <- allData[last_soilMes >= input$lastMeas]
    siteVdl <- selTab$vdlName[selTab$VDL_ID %in% input$selByVdl]
    siteClass <- selTab$vdlName[selTab$CLASS %in% input$selByClass]
    # siteSel <- input$dataset
    if(is.null(input$selByVdl) & is.null(input$selByClass)){
      sites <- unique(allData$vdlName)
      siteX = F; sitesSel=NULL
    }else if(input$selSens == "and"){
      sites <- sitesSel <- intersect(siteVdl,siteClass)
      siteX = T
    }else{
      sites <- sitesSel <- unique(c(siteVdl,siteClass))
      siteX = T
    }
    sites <- intersect(sites,unique(subData$vdlName))
    if(length(input$map_marker_click$lng)>0){
      df <- selTab %>% filter(LON == input$map_marker_click$lng &
                                LAT == input$map_marker_click$lat)
      siteClickNew <- df$vdlName
      # print(siteClickNew)
      # print(siteClick)
      if(any(siteClickNew %in% siteClick)){
        #   # print("passed")
        siteClick <<- siteClick[!siteClick %in% siteClickNew]
        #   print(siteClick)
      } else{
        siteClick <<- c(siteClick,siteClickNew)  
      }
      
      # if(length(duplicated(siteClick))>0) siteClick <<- siteClick[!siteClick %in% siteClick[duplicated(siteClick)]]
      
      if(siteX){
        sites <- sitesSel <- unique(c(sites,siteClick))
        # sitesSel <<- siteClick
      } else{
        sitesSel <- siteClick
      }
    }
    nSites <<- length(sites)
    updateCheckboxGroupInput(session, "dataset",
                             label = "Choose sensors:",
                             choices = sort(sites),
                             selected = sitesSel)
  })
  
  
  
  output$distPlot <- renderPlot({
    sites <- input$dataset
    subData <- allData[last_soilMes >= input$lastMeas]
    subData <- subData[dates %between% c(input$startdate, input$enddate)]
    subData <- subData[vdlName %in% sites]
    if(nrow(subData)>1) subData[,dates:=cut(subData$dates, breaks=input$timestep)]
    subData <- subData[, lapply(.SD, mean, na.rm=TRUE), by=list(vdlName,dates),
                       .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ]
    subData <- subData %>% group_by(vdlName) %>%
      mutate(dSM = order_by(dates, soil_moisture_percent - lag(soil_moisture_percent)))
    
    
    subData$vdlName <- factor(subData$vdlName)
    subData$dates <- as.Date(subData$dates)
    
    plot1 <- ggplot(data=subData,
                    aes_string(x = input$Xaxis, y = input$Yaxis,group="vdlName",color="vdlName", shape="vdlName")) +
      scale_shape_manual(values=1:nlevels(subData$vdlName)) +
      xlab(input$Xaxis) +
      ylab(input$Yaxis) +
      # scale_x_date(labels = date_format("%m-%Y"))+
      # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      # scale_x_discrete(breaks = round(seq(as.Date(input$startdate), as.Date(input$enddate),
      #                                       length.out = 5),1)) +
      # geom_line()
      geom_point() +
      labs(caption = paste(length(unique(subData$vdlName)), "of",
                           nSites, "available sensors"))
    
    print(plot1)
  },height = 400,width = 600)  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server,options = list(height = 600))



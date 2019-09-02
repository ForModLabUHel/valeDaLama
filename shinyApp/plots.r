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

# Define server logic required to draw a histogram ----
server <- function(input, output,session) {
  
  observe({
    subData <<- allData[last_soilMes >= input$lastMeas]
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
      if(siteX){
        sites <- sitesSel <- unique(c(sites,df$vdlName))
      } else{
        sites <- sitesSel <- df$vdlName
      }
    }
    nSites <<- length(sites)
    updateCheckboxGroupInput(session, "dataset",
                             label = "Choose sensors:",
                             choices = sort(sites),
                             selected = sitesSel)
  })
  
  observe({
    proxy <- leafletProxy("map", data = subData)
    proxy %>% clearMarkers()
    if (nSites>0) {
      proxy %>% addCircleMarkers(stroke = FALSE,  fillOpacity = 0.2) #%>%
        # addLegend("bottomright", pal = pal2, values = data$depth_type,
        #           title = "Depth Type",
        #           opacity = 1)}
    }else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("map", data = subData)
    proxy %>% clearMarkers()
    if (nSites>0) {
      proxy %>%  addHeatmap(lng=~LON, lat=~LAT,  blur =  10, max = 0.05, radius = 15) 
    }
    else{
      proxy %>% clearHeatmap()
    }
    
    
  })
  
  
  
  output$map <- renderLeaflet({
    sites <- input$dataset
    #subData <- allData[last_soilMes >= input$lastMeas]
    subData <- subData[dates %between% c(input$startdate, input$enddate)]
    subData <- subData[vdlName %in% sites]
    if(nrow(subData)>1) subData[,dates:=cut(subData$dates, breaks=input$timestep)]
    subData <- subData[, lapply(.SD, mean, na.rm=TRUE), by=list(vdlName,dates),
                       .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ]
    subData <- subData %>% group_by(vdlName) %>%
      mutate(dSM = order_by(dates, soil_moisture_percent - lag(soil_moisture_percent)))
    
    
    subData$vdlName <- factor(subData$vdlName)
    subData$dates <- as.Date(subData$dates)
    subCoord <- selTab[which(selTab$vdlName %in% unique(subData$vdlName)),.(LAT,LON)]
    
    lmap <- leaflet() %>%
      # setView(lat = 37.13735,lat2=37.14682, lng1=  -8.641081,lng2=-8.621029) %>%  # set map view
      addTiles() %>%   
      fitBounds(-8.641081, 37.13735, -8.621029, 37.14682)  %>% 
      # addRasterImage(plotRGB(df2015)) %>%# Add default OpenStreetMap map background tiles
      addCircleMarkers(lat = selTab$LAT, lng = selTab$LON, label=selTab$vdlName,radius = 3) %>% 
      addCircles(lat = subCoord$LAT, lng = subCoord$LON, col=2,label=selTab$vdlName) #%>% 
      #addMarkers(lng = selTab$LON, lat = selTab$LAT)
  })
  
  output$distPlot <- renderPlot({
    
    
    sites <- input$dataset
    #subData <- allData[last_soilMes >= input$lastMeas]
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
    
    # plot2 <- vdlMap + 
    #   geom_point(data=selTab,aes(x=LON,y=LAT),color="light blue") + 
    #   geom_point(data=subCoord,aes(x=LON,y=LAT),color="red",size=3) +
    #   ylab("") +xlab("")
    
    print(plot1)
  },height = 400,width = 600)  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server,options = list(height = 600))



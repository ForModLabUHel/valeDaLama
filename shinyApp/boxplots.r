
### @knitr boxplots
# Define UI for app that draws a boxplot ----
uiBoxP <- fluidPage(

  # # App title ----
  # titlePanel("Vale da lama GROW sensors"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Choose variable:",
                  choices = c("light", "air_temperature_celsius",
                              "soil_moisture_percent","dSM"),
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
                  selected = min(unique(floor_date(allData$dates, "day")))),
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
                         choices=c("all sensors",unique(allData$vdlName)),
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
serverBoxP <- function(input, output,session) {

  observe({
    subData <- allData[last_soilMes >= input$lastMeas]
    siteVdl <- selTab$vdlName[selTab$VDL_ID %in% input$selByVdl]
    siteClass <- selTab$vdlName[selTab$CLASS %in% input$selByClass]
    # siteSel <- input$dataset
    if(is.null(input$selByVdl) & is.null(input$selByClass)){
      sites <- unique(allData$vdlName)
    }else if(input$selSens == "and"){
      sites <- intersect(siteVdl,siteClass)
    }else{
      sites <- unique(c(siteVdl,siteClass))
    }
    sites <- c("all sensors",intersect(sites,unique(subData$vdlName)))
    nSites <<- length(sites)-1
    updateCheckboxGroupInput(session, "dataset",
                             label = "Choose sensors:",
                             choices = sort(sites),
                             selected = "all sensors")
  })
  output$distPlot <- renderPlot({

    sites <- input$dataset
    selSites <- length(sites)
    subData <- allData[dates %between% c(input$startdate, input$enddate)]
    if("all sensors" %in% sites){
      selSites <- selSites - 1
      subData[,dates:=cut(subData$dates, breaks=input$timestep)]
      subData <- subData[, lapply(.SD, mean, na.rm=TRUE), by=list(vdlName,dates),
                         .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ]
      subData[,dSM := order_by(dates, soil_moisture_percent - lag(soil_moisture_percent)),by=vdlName]
      subData$vdlName <- factor(subData$vdlName)
      p1 <- ggplot(data=subData,
                   aes_(x = "all sensors", y = as.name(input$variable))) +
        xlab("") + ylab("") +
        geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      plot1 <- p1 + geom_boxplot(data=subData[vdlName %in% sites],
                        aes_string(x = "vdlName", y = input$variable,group="vdlName",
                                   color="vdlName", shape="vdlName")) +
        scale_shape_manual(values=1:nlevels(subData$vdlName)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(caption = paste(selSites, "of",
                             nSites, "available sensors"))
      subCoord <- selTab[which(selTab$vdlName %in% unique(subData[vdlName %in% sites]$vdlName)),.(LAT,LON)]
    }else{
      subData    <- subData[vdlName %in% sites]
      if(nrow(subData)>1) subData[,dates:=cut(subData$dates, breaks=input$timestep)]
      subData <- subData[, lapply(.SD, mean, na.rm=TRUE), by=list(vdlName,dates),
                         .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ]
      subData <- subData %>% group_by(vdlName) %>%
        mutate(dSM = order_by(dates, soil_moisture_percent - lag(soil_moisture_percent)))
  
      subData$vdlName <- factor(subData$vdlName)
      # subData$dates <- as.Date(subData$dates)
  
      plot1 <- ggplot(data=subData,
             aes_string(x = "vdlName", y = input$variable,group="vdlName",color="vdlName", shape="vdlName")) +
        scale_shape_manual(values=1:nlevels(subData$vdlName)) +
        xlab("") +
        ylab(input$variable) +
        geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(caption = paste(selSites, "of",
                             nSites, "available sensors"))
      subCoord <- selTab[which(selTab$vdlName %in% unique(subData$vdlName)),.(LAT,LON)]
    }
  plot2 <- vdlMap + 
    geom_point(data=selTab,aes(x=LON,y=LAT),color="light blue") + 
    ylab("") +xlab("")+ geom_point(data=subCoord,aes(x=LON,y=LAT),color="red",size=3)
  grid.arrange(plot1, plot2, nrow=2)
  },height = 800,width = 600)  
  

}
# 
# 
# Create Shiny app ----
shinyApp(ui = uiBoxP, server = serverBoxP,options = list(height = 600))


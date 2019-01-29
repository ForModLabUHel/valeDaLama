
## @knitr violin
# Define UI for app that draws a boxplot ----
uiViolin <- fluidPage(

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
                  choices = unique(floor_date(allData$dates, "day"))),
      selectInput(inputId = "enddate",
                  label = "Choose end date:",
                  choices = unique(ceiling_date(allData$dates, "day")),
                  selected = max(unique(ceiling_date(allData$dates, "day")))),
      selectInput(inputId = "lastMeas",
                  label = "Choose sensors according to last measurements date:",
                  choices = sort(unique(floor_date(allData$last_soilMes, "day"))),
                  selected = max(unique(floor_date(allData$dates, "day")))),
      selectizeInput(inputId = "selByClass",
                     label = "select sensors by class",
                     choices = unique(selTab$CLASS), multiple = TRUE),
      selectInput(inputId = "selSens",
                  label = "",
                  choices = c("or","and")),
      selectizeInput(inputId = "selByVdl",
                     label = "select sensors by vdl_id",
                     choices = unique(selTab$VDL_ID), multiple = TRUE),
      checkboxGroupInput(inputId = "dataset", label = "Choose sensors:",
                         choices=c("all sensors",unique(allData$longName)),
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
serverViolin <- function(input, output,session) {
  
  observe({
    subData <- allData[last_soilMes >= input$lastMeas]
    siteVdl <- selTab$longName[selTab$VDL_ID %in% input$selByVdl]
    siteClass <- selTab$longName[selTab$CLASS %in% input$selByClass]
    # siteSel <- input$dataset
    if(is.null(input$selByVdl) & is.null(input$selByClass)){
      sites <- unique(allData$longName)
    }else if(input$selSens == "and"){
      sites <- intersect(siteVdl,siteClass)
    }else{
      sites <- unique(c(siteVdl,siteClass))
    }
    sites <- c("all sensors",intersect(sites,unique(subData$longName)))
    nSites <<- length(sites)-1
    updateCheckboxGroupInput(session, "dataset",
                             label = "Choose a sensor:",
                             choices = sort(sites),
                             selected = "all sensors")
  })  
  output$distPlot <- renderPlot({

    # x    <- allData[id==input$dataset,dates]
    # print(input$variable)

    sites <- input$dataset
    selSites <- length(sites)
    subData <- allData[dates %between% c(input$startdate, input$enddate)]
    if("all sensors" %in% sites){
      selSites <- selSites - 1
      subData[,dates:=cut(subData$dates, breaks=input$timestep)]
      subData <- subData[, lapply(.SD, mean, na.rm=TRUE), by=list(longName,dates),
                         .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ]
      subData[,dSM := order_by(dates, soil_moisture_percent - lag(soil_moisture_percent)),by=longName]
      subData$longName <- factor(subData$longName)
      p1 <- ggplot(data=subData,
                   aes_(x = "all sensors", y = as.name(input$variable))) +
        xlab("") + ylab("") +
        geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      p1 + geom_violin(data=subData[longName %in% sites],
                        aes_string(x = "longName", y = input$variable,group="longName",
                                   color="longName", shape="longName"),draw_quantiles = c(0.25,0.5,0.75)) +
        scale_shape_manual(values=1:nlevels(subData$longName)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(caption = paste(selSites, "of",
                             nSites, "available sensors"))
    }else{
      subData    <- subData[longName %in% sites]
      if(nrow(subData)>1) subData[,dates:=cut(subData$dates, breaks=input$timestep)]
      subData <- subData[, lapply(.SD, mean, na.rm=TRUE), by=list(longName,dates),
                         .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ]
      subData <- subData %>% group_by(longName) %>%
        mutate(dSM = order_by(dates, soil_moisture_percent - lag(soil_moisture_percent)))

      subData$longName <- factor(subData$longName)
      # subData$dates <- as.Date(subData$dates)

      ggplot(data=subData,
             aes_string(x = "longName", y = input$variable,group="longName",color="longName", shape="longName")) +
        scale_shape_manual(values=1:nlevels(subData$longName)) +
        xlab("") +
        ylab(input$variable) +
        geom_violin(draw_quantiles = c(0.25,0.5,0.75)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(caption = paste(selSites, "of",
                             nSites, "available sensors"))
    }
  })

}
#
#
# Create Shiny app ----
shinyApp(ui = uiViolin, server = serverViolin,options = list(height = 600))


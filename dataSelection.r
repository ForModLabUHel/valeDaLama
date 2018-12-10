# source("utilis.r");source("plots&maps_utils.R")
library(data.table)

ancData <- fread("data/ancData.txt")

allData <- fread("C:/Users/minunno/Documents/vdlData/processedData/allData.csv")
allData$dates <- as.POSIXct(allData$dates)
date1 <- as.Date("2018-10-01")
date2 <- as.Date("2018-10-24")


###subset dataset by management
sites=ancDataX$longName[which(grepl("MB",ancDataX$longName))]
subDataX <- allData[longName %in% sites]
=======
dataX <- fread("/Users/walterludwick/Documents/data_vdl/allData.csv")
dataX$dates <- as.POSIXct(dataX$dates)
date1 <- as.Date("2018-10-29")
date2 <- as.Date("2018-11-10")


###subset dataset
##extract measurements date range
subDataX <- subset(subDataX, dates > date1 & dates < date2)

###hour mean
subDataX[,hour:=as.POSIXlt(subDataX$dates)$hour]
hourMean <- subDataX[,mean(soil_moisture_percent),by=list(id,hour)]

###different time steps (e.g.,3 hours)
subDataX[,hourX:=cut(subDataX$dates, breaks="1 hours")]
hourXmean <- subDataX[,mean(soil_moisture_percent),by=list(id,hourX)]


##extract measurements at certain time
subDataX <- subset(subDataX, format(dates,'%H:%M')=='06:00' | format(dates,'%H:%M')=='16:00')
dates <- unique(subDataX$dates)

# # dates
# # Step 1: Make Plots For dates Range 
# dates <- unique(dataX$dates)
# dates %>% 
#   map_df(soilM_map)
# 
# 
# # Step 2: List those Plots, Read them in, and then make animation
# list.files(path = "data/maps/temp/", pattern = "*.png", full.names = T) %>% 
#   map(image_read) %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=2) %>% # animates, can opt for number of loops
#   image_write("maps/timeSeries.gif") # write to current dir
# 
# 
# ###delet temporary maps
# # unlink("data/maps/temp/*")
source("utils.r")

newData <- data.table()
files <- list.files(path= "data/collectedData/",pattern = "\\.csv$", recursive = TRUE)
fieldNames <- c("id","capture_datetime_utc","fertilizer_level","light","soil_moisture_percent","air_temperature_celsius","dates","LAT","LON","longName")

for(i in 1:length(files)){
  dataX <- fread(paste0("data/collectedData/",files[i]))
  fileX  <- str_split(files[i],"\\. |\\.| ")
  dataX[,id:=fileX[[1]][3]]
  newData <- rbind(newData,dataX)
}

###read ancilary data
ancData <- fread("data/ancData.txt")
# newData[id=="57E5",id:= ancData$FP_ID[21]]
### and take IDs that are in common 
sitesX <- intersect(ancData$FP_ID,newData$id)

###consider only sites that are in common (siteX)
ancDataX <- ancData[which(ancData$FP_ID %in% sitesX)]
dataX <- newData[which(newData$id %in% sitesX)]

###transform date and time data into posix class object
dataX$dates <-as.POSIXct(dataX$capture_datetime_utc)
###round dates at 15 minutes
dataX$dates <- round_date(dataX$dates,"15 minutes")

ancDataX[, longName:= do.call(paste,.SD), .SDcols=-c(4:9)]

###merge dataX and coordinates
setkey(dataX,"id")
names(ancDataX)[1] <- 'id'
setkey(ancDataX,"id")
dataX <- merge(dataX,ancDataX[,c(1,4,5,10)],by="id")


dataX$dates <- as.character(dataX$dates)

dataX[,..fieldNames]
####merge two readings and remove duplicates
oldData <- fread("data/output/allData.csv")
allData <- rbind(oldData, dataX[,..fieldNames])
allData <- setkey(allData, NULL)
allData <- unique(allData)

##write the new allData files
fwrite(allData, file = "data/output/allData.csv")



##compute daily mean
allData$dates <- as.POSIXct(allData$dates)
dates <- unique(allData$dates)

dailyMean <- allData %>%
  mutate(dates = floor_date(dates,unit="day")) %>%
  group_by(dates,id) %>%
  summarize(soil_moisture_percent = mean(soil_moisture_percent))

dailyMean <- data.table(dailyMean)
dailyData <- merge(dailyMean,ancDataX[,c(1,4,5,10)],by="id")

dailyData$dates <- as.character(dailyData$dates)
fwrite(dailyData, file = "data/output/dailyData.csv")



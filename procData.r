library(lubridate)
library(stringr)
library(data.table)
library(dplyr)
# source("utils.r")

path <- "/Users/walterludwick/Dropbox/sensing_mission/data_vdl/" #wl
# path <- "C:/Users/minunno/Documents/vdlData/" #fm
folderNewData <- paste0(path,"newDataCollection/")
newData <- data.table()
files <- list.files(path= folderNewData,pattern = "\\.csv$", recursive = TRUE)
fieldNames <- c("id","capture_datetime_utc","fertilizer_level","light","soil_moisture_percent","air_temperature_celsius","dates","LAT","LON","longName")

for(i in 1:length(files)){
  dataX <- fread(paste0(folderNewData,files[i]))
  fileX  <- str_split(files[i],"\\. |\\.| ")
  dataX[,id:=fileX[[1]][3]]
  newData <- rbind(newData,dataX)
}


## walt's comment

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
# datesAll <- as.data.table(datesAll)
# setnames(datesAll,"dates")
# dataX$dates <- as.character(dataX$capture_datetime_utc)
# dataX$dates <- as.character(dataX$dates)
dataX$dates <- as.character(dataX$dates)

###If running for the firt time
# fwrite(dataX,paste0(path,"processedData/allData.csv"))

# dataX[,..fieldNames]
####merge two readings and remove duplicates
oldData <- fread(paste0(path,"processedData/allData.csv"))
allData <- rbind(oldData[,..fieldNames], dataX[,..fieldNames])
allData <- setkey(allData, NULL)
allData <- unique(allData)


###Quality report
allData$dates <-as.POSIXct(allData$dates)
datesAll <- seq.POSIXt(min(allData$dates), max(allData$dates), by = "15 min")
datesAll <- as.data.table(datesAll); setnames(datesAll,"dates")
setkey(allData,"dates");setkey(datesAll,"dates")

# allData <- merge(dataX,datesAll,by="dates")

myData <- list()
for(i in unique(allData$id)) myData[[i]] <- merge(allData[id==i],datesAll,by="dates",all=T)


nMeas <- length(datesAll$dates)
resumeTab <- as.data.table(unique(allData$id))
setnames(resumeTab,"id")
resumeTab[,first_soilMes:=as.POSIXct(NA)]
resumeTab[,last_soilMes:=as.POSIXct(NA)]
resumeTab[,nNAs_soilMes:=NA_integer_]
resumeTab[,NAsRatio_soilMes:=NA_real_]
for(i in unique(allData$id)){
  rangeX <- range(which(!is.na(myData[[i]]$soil_moisture_percent)))
  resumeTab[id==i,first_soilMes:=myData[[i]]$dates[rangeX[1]]]
  resumeTab[id==i,last_soilMes:=myData[[i]]$dates[rangeX[2]]]
  resumeTab[id==i,nNAs_soilMes:=sum(is.na(myData[[i]]$soil_moisture_percent))-rangeX[1]]
  resumeTab[id==i,NAsRatio_soilMes:=(sum(is.na(myData[[i]]$soil_moisture_percent))-rangeX[1])/(nMeas-rangeX[1])]
} 


##write the new allData files
allData$dates <- as.character(allData$dates)
fwrite(allData,paste0(path,"processedData/allData.csv"))
fwrite(resumeTab,paste0(path,"processedData/qualCheck.csv"))
# 
# 
# 
# ##compute daily mean
dailyData <- data.table()
for(i in unique(allData$id)){
  dailyMean <- myData[[i]] %>%
    mutate(dates = floor_date(dates,unit="day")) %>%
    group_by(dates) %>%
    summarize(soil_moisture_percent = mean(soil_moisture_percent,na.rm=T))
  
  NAs_Ratio <- myData[[i]] %>%
    mutate(dates = floor_date(dates,unit="day")) %>%
    group_by(dates) %>%
    summarize(NAs_Ratio = sum(is.na(soil_moisture_percent))/length(soil_moisture_percent))
  
  # 
  dailyMean <- data.table(dailyMean)
  NAs_Ratio <- data.table(NAs_Ratio)
  dailyMean <- merge(dailyMean,NAs_Ratio)
  dailyMean[,id:= i]
  dailyMean$soil_moisture_percent[which(is.nan(dailyMean$soil_moisture_percent))] <- NA
  dailyMean$dSM[2:nrow(dailyMean)] = dailyMean$soil_moisture_percent[2:nrow(dailyMean)]-dailyMean$soil_moisture_percent[1:(nrow(dailyMean)-1)]
  dailyData <- rbind(dailyData,dailyMean)
}

dailyData$dates <- as.character(dailyData$dates)
fwrite(dailyData, file = paste0(path,"processedData/dailyData.csv"))


####Move processed csv file to storedData folder and remove original folder
files <- list.files(path= folderNewData)
foldName <- paste0(path,"storedData/processed_", Sys.Date())
dir.create(foldName)
file.copy(paste0(folderNewData,files), 
          paste0(foldName,"/"), recursive=TRUE)
unlink(paste0(folderNewData,files), recursive=TRUE)

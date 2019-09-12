library(lubridate)
library(curl)
library(stringr)
library(data.table)
library(dplyr)
# source("utils.r")



path <- "/Users/walterludwick/Dropbox/sensing_mission/data_vdl/" #wl
# path <- "C:/Users/minunno/Documents/data_vdl/" #fm
folderNewData <- paste0(path,"newDataCollection/")
newData <- data.table()
files <- list.files(path= folderNewData,pattern = "\\.csv$", recursive = TRUE)
fieldNames <- c("id","capture_datetime_utc","fertilizer_level","light","soil_moisture_percent","air_temperature_celsius","dates","LAT","LON")

for(i in 1:length(files)){
  dataX <- fread(paste0(folderNewData,files[i]))
  fileX  <- str_split(files[i],"\\. |\\.| ")
  dataX[,id:=fileX[[1]][3]]
  newData <- rbind(newData,dataX)
}


## walt's comment
#################################################################################
###read ancilary data
###read and process ancDataAT.csv
selTab <- fread("~/Dropbox/sensing_mission/data_vdl/processedData/ancDataAT.csv")
# selTab <- fread("C:/Users/minunno/GitHub/valeDaLama/data/ancDataAT.csv")
selTab$LAT <- as.numeric(substr(selTab$LAT,1,10))
selTab$LON <- as.numeric(substr(selTab$LON,1,10))
selTab$last_soilMes <- as.Date(selTab$last_soilMes,format = "%Y-%m-%d %H:%M:%S")
selTab[,vdlName:=paste(VDL_ID,VDL_CLASS)]
selTab <- selTab[vdlName != " "]

##to check with Walt
# selTab <- selTab[-which(duplicated(selTab$vdlName))] ###remove sensors with no vdlName
dupX <- selTab$vdlName[which(duplicated(selTab$vdlName))]
repSens <- data.table()
for(i in 1:length(dupX)){
  dupSes <- selTab[vdlName==dupX[i]]
  dupSes$last_soilMes <- as.Date(dupSes$last_soilMes,format = "%Y-%m-%d %H:%M:%S")
  repSensX <- dupSes[which.max(dupSes$last_soilMes)]
  repSensX$FROM_DATETIME <- dupSes[which.min(dupSes$last_soilMes)]$FROM_DATETIME
  repSensX$first_soilMes <- dupSes[which.min(dupSes$last_soilMes)]$first_soilMes
  repSens <- rbind(repSens,repSensX)
}
selTab <- selTab[! vdlName %in% dupX]
selTab <- rbind(selTab,repSens)

ancData <- selTab
selTab <- selTab[,.(vdlName,VDL_ID,VDL_CLASS,LAT,LON)]
#################################################################################

setnames(ancData,"FP_ID","id")

sitesX <- intersect(ancData$id,newData$id)

###consider only sites that are in common (siteX)
ancDataX <- ancData[which(ancData$id %in% sitesX)]
dataX <- newData[which(newData$id %in% sitesX)]

###transform date and time data into posix class object
dataX$dates <-as.POSIXct(dataX$capture_datetime_utc, format="%Y-%m-%d %H:%M:%S",tz="GMT")
###round dates at 15 minutes
dataX$dates <- round_date(dataX$dates,"15 minutes")
# ancDataX[, longName:= do.call(paste,.SD), .SDcols=-c(4:9)]
# ancDataX[, longName:= do.call(paste,.SD), .SDcols=c(2,3,1)]

###merge dataX and coordinates
setkey(dataX,"id")
names(ancDataX)[1] <- 'id'
setkey(ancDataX,"id")
dataX <- merge(dataX,ancDataX[,.(id,LAT,LON,vdlName)],by="id")
# datesAll <- as.data.table(datesAll)
# setnames(datesAll,"dates")
# dataX$dates <- as.character(dataX$capture_datetime_utc)

# dataX$dates <- as.character(dataX$dates)

###If running for the firt time
# fwrite(dataX,paste0(path,"processedData/allData.csv"))

# dataX[,..fieldNames]
####merge two readings and remove duplicates
load(paste0(path,"processedData/allData.rdata"))
oldData <- allData
# oldData <- fread(paste0(path,"processedData/allData.csv"))
# oldData$vdlName <- sub("^(\\S*\\s+\\S+).*", "\\1", oldData$longName)

# oldData$dates <-as.POSIXct(oldData$capture_datetime_utc, format="%Y-%m-%d %H:%M:%S",tz="Europe/London")
# oldData$dates <- round_date(oldData$dates,"15 minutes")

allData <- rbind(oldData[,..fieldNames], dataX[,..fieldNames])
allData <- setkey(allData, NULL)
allData <- unique(allData)


###Quality report
# allData$dates <-as.POSIXct(allData$dates)
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




####update data for shiny app
# allData$dates <- as.POSIXct(allData$dates)
# allData$longName[which(allData$longName=="07AA SS58")] <- "07AA SS58 GRID"
# nameIDs <- matrix(unlist(strsplit(allData$longName," ")),nrow(allData),3,byrow = T)
# reordNames <- paste(nameIDs[,2],nameIDs[,3],nameIDs[,1])
# allData$longName <- reordNames

# resumeTab$last_soilMes <- as.POSIXct(resumeTab$last_soilMes)
setkey(allData,"id")
allData <- merge(allData,resumeTab[,c(1:3)])
allData <- merge(allData,ancData[,.(id,vdlName)])

startDates <- sort(unique(floor_date(allData$dates, "day")))
endDates <- sort(unique(ceiling_date(allData$dates, "day")),decreasing = T)
maxEndDates <- max(endDates)
lastSoilMeass <- sort(unique(floor_date(allData$last_soilMes, "day")))
minLastSoilMeas <- min(lastSoilMeass)

dailyData <- allData
dailyData[,dateX:=cut(dailyData$dates, breaks="1 d")]
dailyData <- dailyData[, lapply(.SD, mean, na.rm=TRUE), by=list(vdlName,dateX),
                       .SDcols=c("light","soil_moisture_percent", "air_temperature_celsius") ]
setkey(dailyData,dateX)
dailyData$vdlName <- factor(dailyData$vdlName)
allData$vdlName <- factor(allData$vdlName)

dailyData[,dSM := lag(soil_moisture_percent),by=vdlName]
dailyData[,dSM := soil_moisture_percent - dSM,by=vdlName]

ops <- allData[,unique(last_soilMes),by=vdlName] 
setnames(ops,"V1", "last_soilMes")
dailyData <- merge(dailyData,ops,by = "vdlName",allow.cartesian = T)
dailyData$dates <- as.Date(dailyData$dateX)

##write the new allData files
save(selTab,dailyData,allData,startDates, endDates, maxEndDates, lastSoilMeass, minLastSoilMeas,file=paste0(path,"processedData/allData.rdata"))
# allData$dates <- as.character(allData$dates)
resumeTab$last_soilMes <- as.character(resumeTab$last_soilMes)
resumeTab$first_soilMes <- as.character(resumeTab$first_soilMes)
# fwrite(allData,paste0(path,"processedData/allData.csv"))
fwrite(resumeTab,paste0(path,"processedData/qualCheck.csv"))



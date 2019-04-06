####this script is to read new file and update the data

library(lubridate);library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(curl)



####update data
allData <- fread( "https://www.dropbox.com/s/ngaexvxlazshb0j/allData.csv?dl=1")
allData$dates <- as.POSIXct(allData$dates)
# allData$longName[which(allData$longName=="07AA SS58")] <- "07AA SS58 GRID"
# nameIDs <- matrix(unlist(strsplit(allData$longName," ")),nrow(allData),3,byrow = T)
# reordNames <- paste(nameIDs[,2],nameIDs[,3],nameIDs[,1])
# allData$longName <- reordNames

   
qualityCheck <- fread( "https://www.dropbox.com/s/z38ffeqv7akraov/qualCheck.csv?dl=1")
qualityCheck$last_soilMes <- as.POSIXct(qualityCheck$last_soilMes)
allData <- merge(allData,qualityCheck[,c(1,3)])

save(allData,file="allData.rdata")
library(viridis) # nice color palette
library(ggmap) # ggplot functionality for maps
library(dplyr) # use for fixing up data
library(readr) # reading in data/csv
library(RColorBrewer) # for color palettes
library(purrr) # for mapping over a function
library(magick) # this is call to animate/read pngs

library(maptools)
library(gpclib)
library(rgdal)
library(ggplot2)
library(sp)
library(RStoolbox)
# library(plotly)
# library(gapminder)
library(data.table)
library(stringr)
library(RgoogleMaps)
library(lubridate)
library(raster)
library(rasterVis)

allData <- data.table()
files <- list.files(path= "data/collectedData/",pattern = "\\.csv$", recursive = TRUE)

for(i in 1:length(files)){
  dataX <- fread(paste0("data/collectedData/",files[i]))
  fileX  <- str_split(files[i],"\\. |\\.| ")
  dataX[,id:=fileX[[1]][5]]
  allData <- rbind(allData,dataX)
}

###read ancilary data
ancData <- fread("data/ancData.txt")
### and take IDs that are in common 
sitesX <- intersect(ancData$FP_ID,allData$id)

###consider only sites that are in common (siteX)
ancDataX <- ancData[which(ancData$FP_ID %in% sitesX)]
dataX <- allData[which(allData$id %in% sitesX)]

###transform date and time data into posix class object
dataX$dates <-as.POSIXct(dataX$capture_datetime_utc)
###round dates at 15 minutes
dataX$dates <- round_date(dataX$dates,"15 minutes")



###merge dataX and coordinates
setkey(dataX,"id")
names(ancDataX)[1] <- 'id'
setkey(ancDataX,"id")
dataX <- merge(dataX,ancDataX[,c(1,4,5)],by="id")



###subset dataset
##extract measurements at certain time
subDataX <- subset(dataX, format(dates,'%H:%M')=='06:00' | format(dates,'%H:%M')=='16:00')
dates <- unique(subDataX$dates)

##compute daily mean
dailyMean <- dataX %>%
  mutate(dates = floor_date(dates,unit="day")) %>%
  group_by(dates,id) %>%
  summarize(mean_SM = mean(soil_moisture_percent))

dailyMean <- data.table(dailyMean)
dates <- unique(dailyMean$dates)


# process images (change coordinates and crop) 
# ortoPhoto <- raster("C:/Users/minunno/Documents/walt/Ortofoto_RGB.tif")
# ops = brick("data/valeDaLama_raster.tif")
# ll <- projectRaster(ortoPhoto, crs=crs(ops))
# ops = raster("data/valeDaLama_raster.tif")
# e <- extent(-8.64,-8.625,37.1375,37.1425)
# rasterZoom <- crop(ops,e)
# writeRaster(rasterZoom,filename = "data/valeDaLama_rasterZoom.tif")

###map settings
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 100))
# dem1 <- raster("data/DEMvdl.tif")
# dem2 <- raster("data/DEMvdl2.tif")
df = brick("data/valeDaLama_raster.tif")


###make maps and gif
ndwi_map <- function(dateX){
  
  ggRGB(df)+
    geom_point(
      
      data=dataX[dates==dateX], mapping = aes(x = LON, y = LAT,  
                                                             colour=soil_moisture_percent),
      size=3) +
    labs(title = "Soil Moisture %",
         subtitle = dateX) + 
     ylim(37.1375,37.142) +
     xlim(-8.6375,-8.629) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())+
    labs(colour = "%") +
  sc
  print(paste0("saving plot ", dateX))
  ggsave(filename = paste0("maps/hgm_ndwi_",as.numeric(dateX),".png"),
         width = 8,height=8,dpi = 150)
}


# dates
# Step 1: Make Plots For dates Range 
dates %>% 
  map_df(ndwi_map)


# Step 2: List those Plots, Read them in, and then make animation
list.files(path = "maps/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("maps/timeSeries.gif") # write to current dir


# qplot(dem1)




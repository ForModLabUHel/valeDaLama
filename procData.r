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

###subset dates
dataX <- subset(dataX, format(dates,'%H:%M')=='06:00' | format(dates,'%H:%M')=='16:00')

dates <- unique(dataX$dates)


###merge dataX and coordinates
setkey(dataX,"id")
names(ancDataX)[1] <- 'id'
setkey(ancDataX,"id")
dataX <- merge(dataX,ancDataX[,c(1,4,5)],by="id")


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





# # allData$id <- factor(allData$id)
# 
# 
# p1 <- ggplot(data=allData, aes(x = dates, y = light,
#                                         group=id,color=id, shape=id)) +
#   scale_shape_manual(values=1:nlevels(allData$id)) +
#   # labs(title = "Policy")+
#   # xlab("Year") +
#   # geom_line() 
#   # ylim(ylim) +
#   theme(legend.position="none") +
#   geom_point(pch=".")
# 
# # p1 
# p2 <- ggplot(data=allData, aes(x = dates, y =soil_moisture_percent,
#                                group=id,color=id, shape=id)) +
#   scale_shape_manual(values=1:nlevels(allData$id)) +
#   # labs(title = "Policy")+
#   # xlab("Year") +
#   # geom_line() 
#   # ylim(ylim) +
#   theme(legend.position="none") +
#   geom_point(pch=".")
# 
# 
# 
# # ancData <- fread("data/ancData.txt")
# # ancData$id <- factor(ancData$id)
# # Xsites <- which(!unique(allData$id) %in% unique(ancData$FP_ID))
# # Xsites2 <- which(!unique(ancData$FP_ID) %in% unique(allData$id))
# 
# 
# 
# ###MAPS###
# # mapSens <- readOGR("SoilSensorMap.kml")

# lyr <- ogrListLayers("C:/Users/minunno/Documents/walt/SoilSensorMap.kml")
# i=1
# mykml <- readOGR("C:/Users/minunno/Documents/walt/SoilSensorMap.kml",lyr[[i]])
# plot(mykml,col=i,xlim=c(-8.650416,-8.600416),ylim=c(37.14,37.15))
# ctLn <- readOGR("C:/Users/minunno/GitHub/valeDaLama/data/02_Geo_contour_50cm.kml")
# 
# # qmap('Portugal',key = "AIzaSyCT68qsuTUafUKHWSfoqA7StuU1VcFdaZg")
# 
# lat <- selData$LAT
# lon <- selData$LON
# center = c(mean(lat), mean(lon));
# zoom <- min(MaxZoom(range(lat), range(lon)));
# 
# 
# Map <- GetMap(center=center, zoom=16,maptype = "satellite",
#               destfile = "MyTile1.png",
#               API_console_key = "AIzaSyCT68qsuTUafUKHWSfoqA7StuU1VcFdaZg")
# 
# 
# PlotOnStaticMap(Map, lat = lat, 
#                        lon = lon, 
#                        destfile = "MyTile1.png", cex=1.5,pch=20,                       
#                        col='red', add=FALSE)
# 
# 
# 
# 
# 
# sitesX <- unique(allData$id)[which(unique(allData$id) %in% ancData$FP_ID)]
# 
# Xdata <- allData[id %in% sitesX]
# Xdata$dates <- as.POSIX(Xdata$dates)
# 
# Xdata$dates <-as.POSIXct(Xdata$dates)
# 
# ###round to 15 minutes to match dates
# Xdata$dates <- round_date(Xdata$dates,"15 minutes")
# dates <- unique(Xdata$dates)
# 
# Xdata[dates==dates[1]]
# 
# setkey(Xdata,"id")
# names(ancData)[3] <- 'id'
# setkey(ancData,"id")
# ops <- merge(Xdata,ancData[,1:3],by="id")
# 
# t4 <- merge(Xdata, ancData, by = "id", all = TRUE)
# 
# 
# 
# library(dplyr)
# 
# dt3 <- full_join(x = Xdata, y = ancData[,1:3], by = "id") %>%
#   arrange(id)
# 
# ciao <- as.data.table(dt3)
# Xdata <- ciao
# datesX <- Xdata$dates[1]
# #####################################################
# 
# dem <- raster("data/DEMvdl.tif")
# backMap <- gplot(demX) + geom_raster(aes(fill=value))
# 
# 
# 
# gplot(demX)+
#   geom_raster(aes(fill=value))+
#   geom_point(
#     
#   data=dataX[dates==dates], mapping = aes(x = LON, y = LAT,  
#                                                       colour=soil_moisture_percent),
#   size=3) +
#   # ylim(37.1375,37.142) +
#   # xlim(-8.6375,-8.629) +
#   sc
#   # coord_fixed(1.3) + 
#   # geom_polygon(color = "black", fill = "gray") + 
#   # geom_polygon(data = ops[simYear==Yr], aes(fill = value), color = "white") +
#   # geom_polygon(color = "black", fill = NA) +
# 
# 
#     data=as.data.frame(pts),
#     aes(x=x,y=y),col="red")+
#   coord_equal()
# 
# 
# 
# theme_set(theme_bw())
# gplot(dem) + geom_tile(aes(fill = value)) +
#   facet_wrap(~ variable) +
#   scale_fill_gradient(low = 'white', high = 'blue') +
#   coord_equal() +
#   ggplot(data = dataX[dates==dates], mapping = aes(x = LON, y = LAT,  
#                                                                   colour=soil_moisture_percent)) + 
#   geom_point(size=3) +sc
# 
#   geom_tile(data = backMap) +
#   geom_tile(data = gplot_dist1, 
#             aes(x = x, y = y, fill = value)) +
#   scale_fill_gradient("Distance",
#                       low = 'yellow', high = 'blue',
#                       na.value = NA)
# 
# 
#   gplot_wrld_r <- gplot_data(wrld_r)
#   gplot_dist1 <- gplot_data(dist1)
#   
#   
#   
# 
# myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
# sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 100))
# 
# ndwi_map <- function(dates,dataX){
#   
#   p= ggplot(data = dataX[dates==dates], mapping = aes(x = LON, y = LAT,  
#                                                               colour=soil_moisture_percent)) + 
#     ylim(37.1375,37.14125) +
#     xlim(-8.6375,-8.63) +
#     # coord_fixed(1.3) + 
#     # geom_polygon(color = "black", fill = "gray") + 
#     # geom_polygon(data = ops[simYear==Yr], aes(fill = value), color = "white") +
#     # geom_polygon(color = "black", fill = NA) +
#     geom_point(size=3) +sc
#   print(paste0("saving plot ", dates))
#   ggsave(filename = paste0("maps/hgm_ndwi_",dates,".png"),
#          width = 8,height=8,dpi = 150)
# }
# 
# 
# # dates
# # Step 1: Make Plots For Year Range 
# dates[1:10] %>% 
#   map_df(ndwi_map)
# 
# 
# # Step 2: List those Plots, Read them in, and then make animation
# list.files(path = "maps/", pattern = "*.png", full.names = T) %>% 
#   map(image_read) %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=2) %>% # animates, can opt for number of loops
#   image_write("ndwi_aug_hgm.gif") # write to current dir
# 
# 
# 
# 
# library(viridis)
# maxVal <- max(maxValue(Base))
# p <- levelplot(Base, 
#                margin=FALSE,                       
#                colorkey=list(
#                  space='bottom',                   
#                  labels=list(at=seq(0,maxVal,by=200), font=4),
#                  axis.line=list(col='black')       
#                ),    
#                par.settings=list(
#                  axis.line=list(col='transparent') 
#                ),
#                scales=list(draw=FALSE),            
#                col.regions=viridis,
#                at=seq(0,maxVal, len=101),
#                main=paste("Year:",(yearX+2013)))
# 
# png(paste("maps/",yearX,".png",sep=''))
# print(p)
# dev.off()
# 
# # Step 2: List those Plots, Read them in, and then make animation
# list.files(path = "mapsEvo/", pattern = "*.png", full.names = T) %>% 
#   map(image_read) %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=2) %>% # animates, can opt for number of loops
#   image_write("EvoV.gif") # write to current dir
# 
# 
# 
# 
# Map <- GetMap(center=center, zoom=18,maptype = "terrain",
#               destfile = "MyTile1.png",
#               API_console_key = "AIzaSyCT68qsuTUafUKHWSfoqA7StuU1VcFdaZg")
# 
# 
# PlotOnStaticMap(Map)
# 
# 
# data = Xdata[dates==datesX][1:10]
# qmplot(LON, LAT, data = data,
#        colour = soil_moisture_percent, size = I(3), darken = .3,api)
# 
# murder <- subset(crime, offense == "murder")
# 
# 
# newmap <- GetMap(center = c(36.7, -5.9), zoom = 10, destfile = "newmap.png", 
#                  maptype = "satellite",
#                  API_console_key = "AIzaSyCT68qsuTUafUKHWSfoqA7StuU1VcFdaZg")
# 
# 
# 
# library(ggmap)
# al1 = get_map(location = c(lon = -86.304474, lat = 32.362563), zoom = 11, maptype = 'roadmap',
#               api_key = "AIzaSyCT68qsuTUafUKHWSfoqA7StuU1VcFdaZg")
# al1MAP = ggmap(al1)
# al1MAP
# 
# 
# geom_map(map=Map)
#   


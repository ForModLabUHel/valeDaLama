library(RStoolbox)
source("utilis.r")
###map settings
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 100))
# scAll <- scale_colour_gradientn(colours = myPalette(ceiling(max(dataX$soil_moisture_percent))), limits=c(0, max(dataX$soil_moisture_percent)))
# scDaily <- scale_colour_gradientn(colours = myPalette(ceiling(max(dailyData$mean_SM))), limits=c(0, max(dailyData$mean_SM)))
df = brick("data/valeDaLama_raster.tif")
df2015 = brick("data/vdl_2015Georef.tif")
df2018 = brick("data/vdl_2018_georef.tif")



###create Map2015
map2015 <- ggRGB(df2015)+
  ylim(37.138,37.1443) +
  xlim(-8.637,-8.625) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  labs(colour = "%")
###create Map2018
map2018 <- ggRGB(df2018)+
  ylim(37.138,37.1443) +
  xlim(-8.637,-8.625) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  labs(colour = "%")
save(map2015,map2018,file="C:/Users/minunno/Documents/data_vdl/processedData/vdlMap.rdata")

ggsave(filename = paste0("data/maps/temp/",dateX,".png"),
       width = 8,height=8,dpi = 150)



###function for Maps
soilM_map <- function(dateX){
  
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
  ggsave(filename = paste0("data/maps/temp/",dateX,".png"),
         width = 8,height=8,dpi = 150)
}

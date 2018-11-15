
###map settings
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 100))
# scAll <- scale_colour_gradientn(colours = myPalette(ceiling(max(dataX$soil_moisture_percent))), limits=c(0, max(dataX$soil_moisture_percent)))
# scDaily <- scale_colour_gradientn(colours = myPalette(ceiling(max(dailyData$mean_SM))), limits=c(0, max(dailyData$mean_SM)))
df = brick("data/valeDaLama_raster.tif")

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

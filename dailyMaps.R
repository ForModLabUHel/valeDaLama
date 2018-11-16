source("utilis.r");source("plots&maps_utils.R")

dataX <- fread("data/output/dailyData.csv")
sc <- scale_colour_gradientn(colours = myPalette(ceiling(max(dataX$soil_moisture_percent))),
                            limits=c(0, max(dataX$soil_moisture_percent)))
  
# dates
# Step 1: Make Plots For dates Range 
dates <- unique(dataX$dates)
dates %>% 
  map_df(soilM_map)


# Step 2: List those Plots, Read them in, and then make animation
list.files(path = "data/maps/temp/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("data/maps/dailySeries.gif") # write to current dir


###delet temporary maps
unlink("data/maps/temp/*")

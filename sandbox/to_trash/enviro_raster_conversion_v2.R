#Objective: read/convert geotiff layers back to ASCII for MAXENT, make sure to recode NA values
#Author: NAH

#load required packages
require(rgeos); require(ggmap); require(sp); require(rgdal); require(raster); require(tidyverse); require(RColorBrewer)

#set working directory MUST CHANGE TO YOUR OWN!
setwd("/Volumes/GoogleDrive/My Drive/spotted_lanternfly/data/environment/MAXENT LAYERS/tifs/v2.2/")

# ---------------------------------------------------------------------------------
# Load Data

#The script here will take the geotiffs called in layers and layers_short and converts them into ASCII format

layers <- list.files(path = ".", pattern = "[.]tif", full.names = T)
layers_short <- list.files(path = ".", pattern = "[.]tif", full.names = F)
layers_short <- gsub(pattern = ".tif", replacement = ".asc", x = layers_short)

for(a in seq_along(layers)){
  print(layers[a])
  file_to_asc <- raster(layers[a])
  NAvalue(file_to_asc) <- -9999
  writeRaster(x = file_to_asc, filename = paste0(getwd(), "/",  layers_short[a]), format = "ascii", overwrite = F)
}

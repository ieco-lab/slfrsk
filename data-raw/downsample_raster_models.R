#downsample Maxent output rasters to plot more easily

#may want to do it straight from ASCII version some day

#Authors: NAH

#load packages
library(tidyverse)
library(rgeos)
library(rgdal)
library(raster)

#set working directory
#setwd("/Volumes/GoogleDrive/My Drive/spotted_lanternfly_ieco_projects/")

mypath <- "/Volumes/GoogleDrive/Shared drives/slfData/data/slfRisk"


#################################
#world
#################################

#read in a geotiff version of the maxent suitability for all three models
enm_data_slftoh <- raster(file.path(mypath, "maxent_models", "slftoh.tif"))
enm_data_toh <- raster(file.path(mypath, "maxent_models", "toh.tif"))
enm_data_slf <- raster(file.path(mypath, "maxent_models", "slf.tif"))

enm_data_mean <- raster(file.path(mypath, "maxent_models", "slftoh_ensemble_mean.tif"))

#stack the rasters
#enm_data <- stack(c(enm_data_slftoh, enm_data_slf, enm_data_toh))

#make mean raster
#enm_data_mean <- mean(enm_data)

#write out the resulting file
#writeRaster(x = enm_data_mean, filename = "./data/geotiff_enms/slftoh_ensemble_mean.tif", format = "GTiff")


#downsample by a factor of 4
enm_data_mean4 <- aggregate(enm_data_mean, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slftoh_ensemble_mean_downsampled_x4.tif"), overwrite = F)
enm_data_slftoh4 <- aggregate(enm_data_slftoh, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slftoh_downsampled_x4.tif"), overwrite = F)
enm_data_slf4 <- aggregate(enm_data_slf, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slf_downsampled_x4.tif"), overwrite = F)
enm_data_toh4 <- aggregate(enm_data_toh, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "toh_downsampled_x4.tif"), overwrite = F)

#same by a factor of 10 from original
enm_data_mean10 <- aggregate(enm_data_mean, fact = 10, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slftoh_ensemble_mean_downsampled_x10.tif"), overwrite = F)
enm_data_slftoh10 <- aggregate(enm_data_slftoh, fact = 10, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slftoh_downsampled_x10.tif"), overwrite = F)
enm_data_slf10 <- aggregate(enm_data_slf, fact = 10, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slf_downsampled_x10.tif"), overwrite = F)
enm_data_toh10 <- aggregate(enm_data_toh, fact = 10, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "toh_downsampled_x10.tif"), overwrite = F)

#################################
#USA (lower 48)
#################################


#get US from world data
world <- readOGR(file.path(mypath, "geo_shapefiles/gadm36_levels_shp/gadm36_0.shp"), verbose = F, p4s = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

#isolate US
usa <- world[world@data$NAME_0 == "United States",]

#crop the models to US
usa_enm_mean <- enm_data_mean %>%
  crop(x = ., y = usa) %>%
  mask(x = ., mask = usa)
usa_enm_slftoh <- enm_data_slftoh %>%
  crop(x = ., y = usa) %>%
  mask(x = ., mask = usa)
usa_enm_toh <- enm_data_toh %>%
  crop(x = ., y = usa) %>%
  mask(x = ., mask = usa)
usa_enm_slf <- enm_data_slf %>%
  crop(x = ., y = usa) %>%
  mask(x = ., mask = usa)

#write out results
writeRaster(x = usa_enm_mean, file.path(mypath, "maxent_models", "slftoh_ensemble_mean_usa.tif"), format = "GTiff")
writeRaster(x = usa_enm_slftoh, filename = file.path(mypath, "maxent_models", "slftoh_usa.tif"), format = "GTiff")
writeRaster(x = usa_enm_toh, filename = file.path(mypath, "maxent_models", "toh_usa.tif"), format = "GTiff")
writeRaster(x = usa_enm_slf, filename = file.path(mypath, "maxent_models", "slf_usa.tif"), format = "GTiff")


#read in USA clipped models (clipping is from geodata.R)
enm_data_slftoh_usa <- raster("./data/geotiff_enms/slftoh_usa.tif")
enm_data_toh_usa <- raster("./data/geotiff_enms/toh_usa.tif")
enm_data_slf_usa <- raster("./data/geotiff_enms/slf_usa.tif")
usa_enm_mean <- raster(file.path(mypath, "maxent_models", "slftoh_ensemble_mean_usa.tif"))


#downsample by a factor of 4
enm_data_mean_usa4 <- aggregate(usa_enm_mean, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slftoh_ensemble_mean_downsampled_x4_usa.tif"), overwrite = F)
enm_data_slftoh_usa4 <- aggregate(usa_enm_slftoh, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slftoh_downsampled_x4_usa.tif"), overwrite = F)
enm_data_toh_usa4 <- aggregate(usa_enm_toh, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "toh_downsampled_x4_usa.tif"), overwrite = F)
enm_data_slf_usa4 <- aggregate(usa_enm_slf, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = file.path(mypath, "maxent_models", "slf_downsampled_x4_usa.tif"), overwrite = F)


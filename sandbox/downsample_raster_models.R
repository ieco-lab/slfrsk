#downsample Maxent output rasters to plot more easily

#may want to do it straight from ASCII version some day

#Authors: NAH

#load packages
library(tidyverse)
library(rgeos)
library(rgdal)
library(raster)

#set working directory
setwd("/Volumes/GoogleDrive/My Drive/spotted_lanternfly_ieco_projects/")

#################################
#world
#################################

#read in a geotiff version of the maxent suitability for all three models
enm_data_slftoh <- raster("./data/geotiff_enms/slftoh.tif")
enm_data_toh <- raster("./data/geotiff_enms/toh.tif")
enm_data_slf <- raster("./data/geotiff_enms/slf.tif")

#stack the rasters
enm_data <- stack(c(enm_data_slftoh, enm_data_slf, enm_data_toh))

#make mean raster
enm_data_mean <- mean(enm_data)

#write out the resulting file
writeRaster(x = enm_data_mean, filename = "./data/geotiff_enms/slftoh_ensemble_mean.tif", format = "GTiff")


#downsample by a factor of 2
enm_data_slftoh2 <- aggregate(enm_data_slftoh, fact = 2, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/slftoh_downsampled_x2.tif", overwrite = F)
enm_data_slf2 <- aggregate(enm_data_slf, fact = 2, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/slf_downsampled_x2.tif", overwrite = F)
enm_data_toh2 <- aggregate(enm_data_toh, fact = 2, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/toh_downsampled_x2.tif", overwrite = F)

#same by a factor of 4 from original
enm_data_slftoh5 <- aggregate(enm_data_slftoh, fact = 5, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/slftoh_downsampled_x5.tif", overwrite = F)
enm_data_slf5 <- aggregate(enm_data_slf, fact = 5, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/slf_downsampled_x5.tif", overwrite = F)
enm_data_toh5 <- aggregate(enm_data_toh, fact = 5, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/toh_downsampled_x5.tif", overwrite = F)

#################################
#USA (lower 48)
#################################

#read in USA clipped models (clipping is from geodata.R)
enm_data_slftoh_usa <- raster("./data/geotiff_enms/slftoh_usa.tif")
enm_data_toh_usa <- raster("./data/geotiff_enms/toh_usa.tif")
enm_data_slf_usa <- raster("./data/geotiff_enms/slf_usa.tif")


#downsample by a factor of 2
enm_data_slftoh_usa2 <- aggregate(enm_data_slftoh_usa, fact = 2, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/slftoh_usa_downsampled_x2.tif", overwrite = F)
enm_data_slf_usa2 <- aggregate(enm_data_slf_usa, fact = 2, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/slf_usa_downsampled_x2.tif", overwrite = F)
enm_data_toh_usa2 <- aggregate(enm_data_toh_usa, fact = 2, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/toh_usa_downsampled_x2.tif", overwrite = F)

#downsample by a factor of 4
enm_data_slftoh_usa4 <- aggregate(enm_data_slftoh_usa, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/slftoh_usa_downsampled_x4.tif", overwrite = F)
enm_data_slf_usa4 <- aggregate(enm_data_slf_usa, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/slf_usa_downsampled_x4.tif", overwrite = F)
enm_data_toh_usa4 <- aggregate(enm_data_toh_usa, fact = 4, fun = mean, expand = TRUE, na.rm = TRUE, filename = "./data/geotiff_enms/toh_usa_downsampled_x4.tif", overwrite = F)


#################################
#now re-read in the downsampled rasters to create the mean model
#################################

#USA
enm_data_slftoh_usa4 <- raster("./data/geotiff_enms/slftoh_usa_downsampled_x4.tif")
enm_data_slf_usa4 <- raster("./data/geotiff_enms/slf_usa_downsampled_x4.tif")
enm_data_toh_usa4 <- raster("./data/geotiff_enms/toh_usa_downsampled_x4.tif")

#stack the rasters
enm_data_usa <- stack(c(enm_data_slftoh_usa4, enm_data_slf_usa4, enm_data_toh_usa4))
#crop to continental USA
enm_data_usa <- raster::crop(enm_data_usa, extent(-124.7628, -66.94889, 24.52042, 49.3833))

#make mean raster
enm_data_usa_mean <- mean(enm_data_usa)

#write out the resulting file
writeRaster(x = enm_data_usa_mean, filename = "./data/geotiff_enms/slftoh_usa_downsampled_x4_mean.tif", format = "GTiff")

#WORLD
enm_data_slftoh4 <- raster("./data/geotiff_enms/slftoh_downsampled_x4.tif")
enm_data_slf4 <- raster("./data/geotiff_enms/slf_downsampled_x4.tif")
enm_data_toh4 <- raster("./data/geotiff_enms/toh_downsampled_x4.tif")

#stack the rasters
enm_data <- stack(c(enm_data_slftoh4, enm_data_slf4, enm_data_toh4))

#make mean raster
enm_data_mean <- mean(enm_data)

#write out the resulting file
writeRaster(x = enm_data_mean, filename = "./data/geotiff_enms/slftoh_downsampled_x4_mean.tif", format = "GTiff")


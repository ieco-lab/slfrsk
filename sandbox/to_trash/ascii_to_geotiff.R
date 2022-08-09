# convert ENM ascii's to geotiffs!

#Authors: NAH

setwd("/Volumes/GoogleDrive/My Drive/spotted_lanternfly_ieco_projects/")

library(rgdal)
library(raster)
library(rgeos)

#read in rasters as asciis
enm_data_slftoh <- raster("./data/Maxent runs/11_07_18_maxent_slf+toh_+atc-bio02/lycorma_delicatula_avg.asc")
enm_data_toh <- raster("./data/Maxent runs/10_29_18_maxent_toh+atc-bio02/Ailanthus_altissima_avg.asc")
enm_data_slf <- raster("./data/Maxent runs/02_15_19_maxent_slf+atc-bio02/lycorma_delicatula_avg.asc")

crs(enm_data_slftoh) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(enm_data_toh) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(enm_data_slf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"



#now convert!
writeRaster(x = enm_data_slftoh, filename = "./data/slf_enm_extract/slftoh.tif", format = "GTiff")
writeRaster(x = enm_data_toh, filename = "./data/slf_enm_extract/toh.tif", format = "GTiff")
writeRaster(x = enm_data_slf, filename = "./data/slf_enm_extract/slf.tif", format = "GTiff")


plot(raster("./data/geotiff_enms/slf.tif"))
plot(raster("./data/geotiff_enms/toh.tif"))

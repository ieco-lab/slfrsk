#cleans raster data for use with MAXENT, downsamples cleaned raster data, and performs correlation calculations on downsampled raster data
#Authors: NAH

#load packages
library(tidyverse)
library(devtools)
library(ENMTools)
library(vegan)

#set working directory
setwd("/Volumes/GoogleDrive/My Drive/spotted_lanternfly_ieco_projects/data/environment/MAXENT LAYERS/tifs/")

#ensure that extent is identical
#get file names
env.files <- list.files("./originals", pattern = "[.]tif", full.names = T)
env.files.short <- list.files("./originals", pattern = "[.]tif", full.names = F)

#change the labeling of the output layers
output.files <- env.files.short

#change weird BIOCLIM prefix
output.files <- gsub(pattern = "wc2.0_bio_30s", replacement = "global_bio", x = output.files)
#change weird ENVIREM prefix
output.files <- gsub(pattern = "current_30arcsec", replacement = "global_env", x = output.files)
#change weird ATC prefix
output.files <- gsub(pattern = "2015_accessibility_to_cities_v1.0", replacement = "global_atc", x = output.files)


#crop one of the BIOCLIM layers to set the bounding box and resolution to have files fixed
same.extent <- extent(-180, 180, -60, 84)
master_layer <- crop(raster("./originals/wc2.0_bio_30s_01.tif"), y = same.extent, overwrite = F)

#store extents and raster res for checking
rasterinfo <- matrix(NA, ncol = 8, nrow = length(env.files))
colnames(rasterinfo) <- c("layer", "xmin", "xmax", "ymin", "ymax", "xres", "yres", "projection")

#reset extent stepwise
for(a in seq_along(env.files)){
  print(env.files.short[a])
  print(output.files[a])
  rasterinfo[a,"layer"] <- output.files[a]
  print(extent(raster(env.files[a])))
  same.extent <- extent(-180, 180, -60, 84)

  #ensure that the CRS is consistent
  rast.hold <- raster(env.files[a])
  crs(rast.hold) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  #resample to fit the extent/resolution of the reference BIOCLIM layer
  #use bilinear interpolation, since values are continuous
  rast.hold <- resample(x = rast.hold, y = raster("./v4/global_bio_01.tif"), method = "bilinear")

  #save summary info for new extent for checking later
  rasterinfo[a, "xmin"] <- extent(rast.hold)@xmin
  rasterinfo[a, "xmax"] <- extent(rast.hold)@xmax
  rasterinfo[a, "ymin"] <- extent(rast.hold)@ymin
  rasterinfo[a, "ymax"] <- extent(rast.hold)@ymax
  rasterinfo[a, "projection"] <- rast.hold@crs@projargs
  rasterinfo[a, c("xres", "yres")] <- res(rast.hold)

  #write out the new resampled rasters!
  writeRaster(x = rast.hold, filename = paste0("./v4/", output.files[a]), overwrite = T)
}

#now we can check that they stack without issue
#load in enviro data
env.files2 <- list.files(path = "./v4", pattern = ".tif", full.names = T)
env <- stack(env.files2)


#downsize raster layers for correlation analysis

#list the enviro layers to load sequentially
env.files <- list.files(path = "./v4", pattern = ".tif", full.names = T)
env.short <- list.files(path = "./v4", pattern = ".tif", full.names = F)

#now let's do it with all of them, including the saving to disk of all downsampled raster layers in a subdirectory
#downsampling by a factor of 2 (read 2 cells deep around a cell) and take the mean of the cells
for(a in seq_along(env.files)){
  holder <- raster(env.files[a])
  down_holder <- aggregate(holder, fact = 2, fun = mean, expand = TRUE, na.rm = TRUE, filename = paste0("./v4_downsampled/", env.short[a]), overwrite = T)
}

#reload the downsampled layers and stack for raster.cor.matrix command
env.files <- list.files(path = "./v4_downsampled", pattern = ".tif", full.names = T)

#stack sequentially to make sure they all have same extent and resolution
for(a in seq_along(env.files)){
  print(env.files[a])
  tester <- stack(env.files[1:a])
  print(res(tester))
  print(tester@extent)
}


#re-stack the downsampled rasters
env <- stack(env.files)
#ensure that boundaries are set for all layers of the raster stack
env <- setMinMax(env)


#evaluate correlations for raster layers
#create a correlation matrix for picking model layers
env.corr <- raster.cor.matrix(env, method = "pearson")

#save the correlation matrix
write.csv(x = env.corr, file = "./env_corr_v4_downsampled.csv", row.names = T, col.names = T)
#version of the same thing, but with rounding
write.csv(round(env.corr, digits = 3), file = "./env_corr_round_4_downsampled.csv", row.names = TRUE)

#re-read the correlation table in again
env.corr <- read.csv(file = "./env_corr_v4_downsampled.csv", row.names = 1)

#create an easy raster correlation plot
cor.plots <- raster.cor.plot(env)

#get distances to do PCoA
env.d <- as.dist(1 - abs(env.corr))

#calculate the PCoA
pcoa <- cmdscale(d = env.d, eig = T)

#Calculate variance on both axes
eigen <- eigenvals(pcoa)

var <- eigen / sum(eigen)

env.d <- as.dist(1 - abs(env.corr))

pcoa <- cmdscale(d = env.d, eig = T)
eigen <- eigenvals(pcoa)

var <- eigen / sum(eigen)

#plot the results
colnames(pcoa$points) <- c("X", "Y")
ggplot2::ggplot(as.data.frame(pcoa$points))  +
  ggplot2::geom_text(ggplot2::aes(x = X, y = Y, label = gsub(rownames(pcoa$points), pattern = "global_", replacement = ""))) +
  ggplot2::theme_bw() +
  ggplot2::xlab(paste0("X - ", (100*round(var[1], digits = 3)), "%")) +
  ggplot2::ylab(paste0("Y - ", (100*round(var[2], digits = 3)), "%"))


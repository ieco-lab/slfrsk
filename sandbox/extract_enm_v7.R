#extract MAXENT data for SLF models per geopolitical unit (country and state)
#authors: NAH

#outline
#1. function that takes the form of: 
  #a. enm = input raster data of ENM
  #b. geoshape = input geo shapefile(s) to obtain data from
  #c. id = indentifier for geo shapefiles to iterate through: a vector of names, may need to match this to a particular part of geoshape
  #d. id0 = part of geoshape@data that holds the names of the geopolitical units to focus on (polygons of interest)
  #e. metric = desired metrics from output (removed)
  #f. th = threshold of suitability for model

#libraries
library(ENMTools)
library(tidyverse)
library(plyr)
library(rgdal)
library(rgeos)
library(viridis)

#set local directory
setwd("/Users/nicholashuron/Google Drive File Stream/My Drive/spotted_lanternfly_ieco_projects/")


#revised version of function that doesn't take metric (just does some summary stats of interest) v2.0
extract_enm <- function(enm, geoshape, id0, id, th, save.plots = FALSE){
  #output object, need to decide on the actual form of it
  output <- {}
  #alternative list object for if save.plots = TRUE
  if(save.plots == TRUE){
    plot.outputs <- vector("list", length = length(id))
  }
  
  #loop to iterate across all instances of id
  for(a in seq_along(unique(id))){
    #obtain subset raster cut to the geoshape of interest
    holder <- raster::extract(x = enm, y = geoshape[geoshape@data[[id0]] %in% unique(id)[a],], df = T)
    
    #plot the density of the distro if requested
    if(save.plots == TRUE){
      p <- ggplot2::ggplot(data = holder) +
        geom_density(aes_string(x = holder[,ncol(holder)], y = '..scaled..')) +
        ggtitle(str_to_title(paste0("Distribution of slf Suitability of ", unique(id)[a]))) +
        xlim(0, 1) +
        labs(x = "suitability", y = "scaled density")
      
      #save to plot.outputs
      plot.outputs[[a]] <- p
    }
    
    #use metric to get the correct summary stat
    #summary stats of interest
    #mean
    holder.mean <- mean(holder[,ncol(holder)], na.rm = T)
    #sd
    holder.sd <- sd(holder[,ncol(holder)], na.rm = T)
    #min (not mentioned by matt)
    holder.min <- min(holder[,ncol(holder)], na.rm = T)
    #max
    holder.max <- max(holder[,ncol(holder)], na.rm = T)
    ##################################################################
    #relative suitable area (RSA)
    #need new argument that is the threshold to use
    holder.rsa <- nrow(holder[holder[,2] >= th,]) / nrow(holder)
    ##################################################################
    #specified quantiles: 
    #25%, median (50%), 75%, 90%
    holder.quants <- quantile(holder[,ncol(holder)], na.rm = T, probs = c(0.25, 0.50, 0.75, 0.90))
    
    #time to figure out what to put in output here
    holder2 <- data.frame(geopol_unit = unique(id)[a],
                          mean = holder.mean, 
                          sd = holder.sd, 
                          min = holder.min, 
                          max = holder.max,
                          rsa = holder.rsa,
                          quantile_0.25 = holder.quants[["25%"]],
                          quantile_0.50 = holder.quants[["50%"]],
                          quantile_0.75 = holder.quants[["75%"]],
                          quantile_0.90 = holder.quants[["90%"]])
    output <- rbind(output, holder2)
  }
  
  #save the plots as a big PDF if save.plots = TRUE
  if(save.plots == TRUE){
    #change the output object to include the plots
    output <- list(summary_stats = output, plots = plot.outputs)  
  }
  else{
    output <- list(summary_stats = output)
  }
  
  #give output of the summary stats
  return(output)
}

#version of the same function that considers the Jung model being scaled [0,100] rather than [0,1]
extract_enm2 <- function(enm, geoshape, id0, id, th, save.plots = FALSE){
  #output object, need to decide on the actual form of it
  output <- {}
  #alternative list object for if save.plots = TRUE
  if(save.plots == TRUE){
    plot.outputs <- vector("list", length = length(id))
  }
  
  #loop to iterate across all instances of id
  for(a in seq_along(unique(id))){
    #obtain subset raster cut to the geoshape of interest
    holder <- raster::extract(x = enm, y = geoshape[geoshape@data[[id0]] %in% unique(id)[a],], df = T)
    
    #plot the density of the distro if requested
    if(save.plots == TRUE){
      p <- ggplot2::ggplot(data = holder) +
        geom_density(aes_string(x = holder[,ncol(holder)], y = '..scaled..')) +
        ggtitle(str_to_title(paste0("Distribution of slf Suitability of ", unique(id)[a]))) +
        xlim(0, 100) +
        labs(x = "suitability", y = "scaled density")
      
      #save to plot.outputs
      plot.outputs[[a]] <- p
    }
    
    #use metric to get the correct summary stat
    #summary stats of interest
    #mean
    holder.mean <- mean(holder[,ncol(holder)], na.rm = T)
    #sd
    holder.sd <- sd(holder[,ncol(holder)], na.rm = T)
    #min (not mentioned by matt)
    holder.min <- min(holder[,ncol(holder)], na.rm = T)
    #max
    holder.max <- max(holder[,ncol(holder)], na.rm = T)
    ##################################################################
    #relative suitable area (RSA)
    #need new argument that is the threshold to use
    holder.rsa <- nrow(holder[holder[,2] >= th,]) / nrow(holder)
    ##################################################################
    #specified quantiles: 
    #25%, median, 75%, 90%
    holder.quants <- quantile(holder[,ncol(holder)], na.rm = T, probs = c(0.25, 0.50, 0.75, 0.90))
    
    #time to figure out what to put in output here
    holder2 <- data.frame(geopol_unit = unique(id)[a],
                          mean = holder.mean, 
                          sd = holder.sd, 
                          min = holder.min, 
                          max = holder.max,
                          rsa = holder.rsa,
                          quantile_0.25 = holder.quants[["25%"]],
                          quantile_0.50 = holder.quants[["50%"]],
                          quantile_0.75 = holder.quants[["75%"]],
                          quantile_0.90 = holder.quants[["90%"]])
    output <- rbind(output, holder2)
  }
  
  #save the plots as a big PDF if save.plots = TRUE
  if(save.plots == TRUE){
    #change the output object to include the plots
    output <- list(summary_stats = output, plots = plot.outputs)  
  }
  else{
    output <- list(summary_stats = output)
  }
  
  #give output of the summary stats
  return(output)
}

##########################################################################################################################################


#load data to test
  #global ENM data (slf based on best toh model)
enm_data_slftoh <- raster("./data/Maxent runs/11_07_18_maxent_slf+toh_+atc-bio02/lycorma_delicatula_avg.asc")
enm_slftoh <- read_csv("./data/Maxent runs/11_07_18_maxent_slf+toh_+atc-bio02/maxentResults.csv")
#the threshold:   enm_slftoh$`Minimum training presence Cloglog threshold`[nrow(enm_slftoh)]
  
  #toh best model
enm_data_toh <- raster("./data/Maxent runs/10_29_18_maxent_toh+atc-bio02/Ailanthus_altissima_avg.asc")
enm_toh <- read_csv("./data/Maxent runs/10_29_18_maxent_toh+atc-bio02/maxentResults.csv")
#the threshold:   enm_toh$`Minimum training presence Cloglog threshold`[nrow(enm_toh)]
  
  #slf based on same env layers as toh best model
enm_data_slf <- raster("./data/Maxent runs/02_15_19_maxent_slf+atc-bio02/lycorma_delicatula_avg.asc")
enm_slf <- read_csv("./data/Maxent runs/02_15_19_maxent_slf+atc-bio02/maxentResults.csv")
#the threshold:   enm_slf$`Minimum training presence Cloglog threshold`[nrow(enm_slf)]

  #load the Jung et al. 2017 model
enm_jung <- raster("./data/geotiff_enms/Jung Lee Korean Models/delicatula_TIFF/Delicatula_PointToRaster32.tif")
enm_jung_th <- 25


  #shapefile of world by countries
world <- readOGR("./data/environment/gadm36_levels_shp/gadm36_0.shp", verbose = T, p4s = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

  #shapefile of USA: data obtained from GADM: https://gadm.org/download_world.html
usa <- readOGR(dsn = "./data/environment/gadm36_USA_shp/gadm36_USA_0.shp", verbose = T)

  #shapefile of US states (same sourcce as usa data)
states <- readOGR(dsn = "./data/environment/gadm36_USA_shp/gadm36_USA_1.shp", verbose = T)


#extract the states
state_extracts_slftoh <- extract_enm(enm = enm_data_slftoh, geoshape = states, id0 = "NAME_1", id = as.character(states$NAME_1), save.plots = T, th = enm_slftoh$`Minimum training presence Cloglog threshold`[nrow(enm_slftoh)])
write_csv(path = "./data/extract_states_11_07_18_maxent_slf+toh+atc-bio02.csv", x = state_extracts_slftoh[[1]], col_names = T)
#try to save as a pdf
pdf(paste0("./data/states_extract_enm_plots_slftoh_", tolower(gsub(pattern = " ", replacement = "_", x = Sys.Date())), ".pdf"))
invisible(lapply(state_extracts_slftoh[[2]], print))
dev.off()

state_extracts_toh <- extract_enm(enm = enm_data_toh, geoshape = states, id0 = "NAME_1", id = as.character(states$NAME_1), save.plots = T, th = enm_toh$`Minimum training presence Cloglog threshold`[nrow(enm_toh)])
write_csv(path = "./data/extract_states_10_29_18_maxent_toh+atc-bio02.csv", x = state_extracts_toh[[1]], col_names = T)
#try to save as a pdf
pdf(paste0("./data/states_extract_enm_plots_toh_", tolower(gsub(pattern = " ", replacement = "_", x = Sys.Date())), ".pdf"))
invisible(lapply(state_extracts_toh[[2]], print))
dev.off()

state_extracts_slf <- extract_enm(enm = enm_data_slf, geoshape = states, id0 = "NAME_1", id = as.character(states$NAME_1), save.plots = T, th = enm_slf$`Minimum training presence Cloglog threshold`[nrow(enm_slf)])
write_csv(path = "./data/extract_states_02_15_19_maxent_slf+atc-bio02.csv", x = state_extracts_slf[[1]], col_names = T)
#try to save as a pdf
pdf(paste0("./data/states_extract_enm_plots_slf_", tolower(gsub(pattern = " ", replacement = "_", x = Sys.Date())), ".pdf"))
invisible(lapply(state_extracts_slf[[2]], print))
dev.off()

#jung model (rescale the plotting in function)
state_extracts_jung <- extract_enm2(enm = enm_jung, geoshape = states, id0 = "NAME_1", id = as.character(states$NAME_1), save.plots = T, th = enm_jung_th)
write_csv(path = "./data/slf_enm_extract/extract_states_jung.csv", x = state_extracts_jung[[1]], col_names = T)
#try to save as a pdf
pdf(paste0("./data/slf_enm_extract/states_extract_enm_plots_jung_", tolower(gsub(pattern = " ", replacement = "_", x = Sys.Date())), ".pdf"))
invisible(lapply(state_extracts_jung[[2]], print))
dev.off()


#countries

#slftoh model
world_extracts_slftoh <- extract_enm(enm = enm_data_slftoh, geoshape = world, id0 = "NAME_0", id = as.character(world$NAME_0), save.plots = T, th = enm_slftoh$`Minimum training presence Cloglog threshold`[nrow(enm_slftoh)])
write_csv(path = "./extract_world_11_07_18_maxent_slf+toh+atc-bio02.csv", x = world_extracts_slftoh[[1]], col_names = T)
pdf(paste0("world_extract_enm_plots_", tolower(gsub(pattern = " ", replacement = "_", x = Sys.Date())), ".pdf"))
invisible(lapply(world_extracts_slftoh[[2]], print))
dev.off()

#toh model
world_extracts_toh <- extract_enm(enm = enm_data_toh, geoshape = world, id0 = "NAME_0", id = as.character(world$NAME_0), save.plots = T, th = enm_toh$`Minimum training presence Cloglog threshold`[nrow(enm_toh)])
write_csv(path = "./extract_world_11_07_18_maxent_toh+atc-bio02.csv", x = world_extracts_toh[[1]], col_names = T)
pdf(paste0("world_extract_enm_plots_", tolower(gsub(pattern = " ", replacement = "_", x = Sys.Date())), ".pdf"))
invisible(lapply(world_extracts_toh[[2]], print))
dev.off()

#slf model
world_extracts_slf <- extract_enm(enm = enm_data_slf, geoshape = world, id0 = "NAME_0", id = as.character(world$NAME_0), save.plots = T, th = enm_slf$`Minimum training presence Cloglog threshold`[nrow(enm_slf)])
write_csv(path = "./data/slf_enm_extract/extract_world_11_07_18_maxent_slf+atc-bio02.csv", x = world_extracts_slf[[1]], col_names = T)
pdf(paste0("./data/slf_enm_extract/world_extract_enm_plots_", tolower(gsub(pattern = " ", replacement = "_", x = Sys.Date())), ".pdf"))
invisible(lapply(world_extracts_slf[[2]], print))
dev.off()

#jung model (need to change the plotting parameters in the function)

  #need to intersect world with jung model to only have countries with data
  #jung_world <- intersect(x = world, y = enm_jung)
  jung_world <- readOGR("./data/environment/jung_model_world.shp", verbose = T, p4s = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

world_extracts_jung <- extract_enm2(enm = enm_jung, geoshape = jung_world, id0 = "NAME_0", id = as.character(jung_world$NAME_0), save.plots = T, th = enm_jung_th)
write_csv(path = "./data/slf_enm_extract/extract_world_jung.csv", x = world_extracts_jung[[1]], col_names = T)
pdf(paste0("./data/slf_enm_extract/world_extract_enm_plots_jung_", tolower(gsub(pattern = " ", replacement = "_", x = Sys.Date())), ".pdf"))
invisible(lapply(world_extracts_jung[[2]], print))
dev.off()


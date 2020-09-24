#' Extracts summary statistics from a raster file based on the boundaries of a shapefile
#'
#' The function \code{extract_enm2}
#'
#'@param enm input raster data (usually a species distribution model, SDM)
#'@param geoshape input shapefile with boundaries of geometries to summarize the raster data for
#'@param id subset in \code{geoshape} identifier table to iterate through: a vector of names, may need to match this to a particular part of geoshape with \code{id0}
#'@param id0 part of \code{'geoshape@data'} that holds the names of the geopolitical units to focus on (polygons of interest)
#'@param th threshold of suitability used to calculate RSA. If not provided, the RSA is not calculated.
#'@param multipar use multicore processors to perform the function in parallel (default is false)
#'@param ncores tell the function to use a set number of cores if \code{multipar = TRUE} and defaults to \code{ncores=1}
#'
#'@return Dataframe (tibble) that contains columns that include
#'@export

extract_enm2 <- function(enm, geoshape, id0, id, th = NA, multipar = FALSE, ncores = 1){

  #multiple processors approach
  if(multipar == TRUE && ncores > 1){
    doParallel::registerDoParallel(cores = ncores)
    print(paste0("Running in parallel with ",foreach::getDoParWorkers(), " cores."))

    if(is.na(th)){
      output <- foreach(a = seq_along(unique(id)), .packages = c('rgeos', 'sp', 'rgdal', 'raster', 'tidyverse'), .combine = rbind) %dopar% {
        #obtain subset raster cut to the geoshape of interest
        holder <- raster::extract(x = enm, y = geoshape[geoshape@data[[id0]] %in% unique(id)[a],], df = T)

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
        #specified quantiles:
        #25%, median, 75%, 90%
        holder.quants <- quantile(holder[,ncol(holder)], na.rm = T, probs = c(0.25, 0.50, 0.75, 0.90))

        #time to figure out what to put in output here
        holder2 <- data.frame(geopol_unit = unique(id)[a],
                              obs_mean = holder.mean,
                              obs_sd = holder.sd,
                              obs_min = holder.min,
                              obs_max = holder.max,
                              quantile_0.25 = holder.quants[["25%"]],
                              quantile_0.50 = holder.quants[["50%"]],
                              quantile_0.75 = holder.quants[["75%"]],
                              quantile_0.90 = holder.quants[["90%"]]
        )
      }
    } else if(!is.na(th)){
      output <- foreach(a = seq_along(unique(id)), .packages = c('rgeos', 'sp', 'rgdal', 'raster', 'tidyverse'), .combine = rbind) %dopar% {
        #obtain subset raster cut to the geoshape of interest
        holder <- raster::extract(x = enm, y = geoshape[geoshape@data[[id0]] %in% unique(id)[a],], df = T)

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
        holder.rsa <- nrow(holder[holder[,2] >= th,]) / nrow(holder)
        ##################################################################
        #specified quantiles:
        #25%, median, 75%, 90%
        holder.quants <- quantile(holder[,ncol(holder)], na.rm = T, probs = c(0.25, 0.50, 0.75, 0.90))

        #time to figure out what to put in output here
        holder2 <- data.frame(geopol_unit = unique(id)[a],
                              obs_mean = holder.mean,
                              obs_sd = holder.sd,
                              obs_min = holder.min,
                              obs_max = holder.max,
                              rsa = holder.rsa,
                              quantile_0.25 = holder.quants[["25%"]],
                              quantile_0.50 = holder.quants[["50%"]],
                              quantile_0.75 = holder.quants[["75%"]],
                              quantile_0.90 = holder.quants[["90%"]]
        )
      }

    }

    #return from parallelized to sequential
    registerDoSEQ()

  } else{
    #loop to iterate across all instances of id
    for(a in seq_along(unique(id))){
      #obtain subset raster cut to the geoshape of interest
      holder <- raster::extract(x = enm, y = geoshape[geoshape@data[[id0]] %in% unique(id)[a],], df = T)


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
      if(!missing(th)){
        holder.rsa <- nrow(holder[holder[,2] >= th,]) / nrow(holder)
      }
      ##################################################################
      #specified quantiles:
      #25%, median (50%), 75%, 90%
      holder.quants <- quantile(holder[,ncol(holder)], na.rm = T, probs = c(0.25, 0.50, 0.75, 0.90))

      #time to figure out what to put in output here
      holder2 <- data.frame(geopol_unit = unique(id)[a],
                            obs_mean = holder.mean,
                            obs_sd = holder.sd,
                            obs_min = holder.min,
                            obs_max = holder.max,
                            quantile_0.25 = holder.quants[["25%"]],
                            quantile_0.50 = holder.quants[["50%"]],
                            quantile_0.75 = holder.quants[["75%"]],
                            quantile_0.90 = holder.quants[["90%"]]
      )

      #optional to use RSA if th is present
      if(!missing(th)){
        holder2 <- rbind(holder2, rsa = holder.rsa)
      }

      output <- rbind(output, holder2)
    }

  }

  #give output of the summary stats
  return(output)
}

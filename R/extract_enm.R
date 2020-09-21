#' Extracts summary statistics from a raster file based on the boundaries of a shapefile
#'
#' The function \code{extract_enm}
#'
#'@param enm input raster data (usually a species distribution model, SDM)
#'@param geoshape input shapefile with boundaries of geometries to summarize the raster data for
#'@param id subset in \code{geoshape} identifier table to iterate through: a vector of names, may need to match this to a particular part of geoshape with \code{id0}
#'@param id0 part of \code{'geoshape@data'} that holds the names of the geopolitical units to focus on (polygons of interest)
#'@param th threshold of suitability used to calculate RSA. If not provided, the RSA is not calculated.
#'@param save.plots save the density plots of points for each \code{id} iterated across (default is FALSE)?
#'
#'@return Dataframe (tibble) that contains columns that include the number of species, the mean at each loss step, the variance at each loss step, and the type of type of simulated extinction. One instance of each type of extinction is run per function call and the resulting simulations are stacked in the same dataframe.
#'
#'@export

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
        ggtitle(str_to_title(paste0("Distribution of Suitability of ", unique(id)[a]))) +
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

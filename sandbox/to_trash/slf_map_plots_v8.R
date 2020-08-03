#Plot MAXENT data as maps for SLF models 
#authors: NAH

#libraries
library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(viridis)
library(ggthemes)
library(gridExtra)
library(plyr)
library(RStoolbox)
library(scales)

#set working directory
setwd("/Users/nicholashuron/Google Drive File Stream/My Drive/spotted_lanternfly_ieco_projects/")

#read in the wineries
wineries <- read_csv(file = "./data/wineries/WineList.csv") %>%
  dplyr::rename(y = `Decimal Lat`, x = `Decimal Long`)

#we need a switch for this map that does the same thing for including the following states: New York, Ohio, West Virginia, North Carolina
present_switch <- FALSE


################################################
#STATES
################################################

#filter for USA plot
usa_wineries <- wineries %>%
  filter(Country == "United States")
  #filter(Country == "United States", Region == "California") 

#read in the centroids for a check
states_centers_wine <- read_csv(file = "./data/wineries/geopolitical_centers_usa.csv") %>%
  dplyr::rename(y = y_coord, x = x_coord)

#SHOULD STATE CENTROIDS BE BASED ON ALL TRADE OR JUST WINE STATES???

#mess with state centers to get a version that has a single point for the invasion core and individual points for the partners
#invasion core: 40.400189, -75.917622
#invaded states: DE, MD, NJ, PA, VA
#future invaded states: NC, NY, OH, WV

#first split for future vs present

if(present_switch == TRUE){
  states_centers2 <- states_centers_wine %>%
    #remove the invaded states
    filter(!geopol_unit %in% c("Delaware", "Maryland", "New Jersey", "Pennsylvania", "Virginia")) %>%
    #remove the others in the dataset
    filter(!is.na(x), !is.na(y)) %>%
    rbind(c("SLF", NA, -75.917622, 40.400189)) %>%
    mutate(avg_wine = as.numeric(avg_wine), x = as.numeric(x), y = as.numeric(y))
  
} else{
  states_centers2 <- states_centers_wine %>%
    #remove the invaded states
    filter(!geopol_unit %in% c("Delaware", "Maryland", "New Jersey", "New York", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "West Virginia")) %>%
    #remove the others in the dataset
    filter(!is.na(x), !is.na(y)) %>%
    rbind(c("SLF", NA, -75.917622, 40.400189)) %>%
    mutate(avg_wine = as.numeric(avg_wine), x = as.numeric(x), y = as.numeric(y))
  
}


####Alt State Centers (capitals)
states_centers <- read_csv(file = "./data/us_capitals.csv") #%>%
  #filter(state %in% unique(states_centers_wine$geopol_unit))

#next version of getting centers
if(present_switch == TRUE){
states_centers2 <- states_centers %>%
  #remove the invaded states
  filter(!state %in% c("Delaware", "Maryland", "New Jersey", "Pennsylvania", "Virginia")) %>%
  #remove the others in the dataset
  filter(!is.na(x), !is.na(y)) %>%
  rbind(c("SLF", NA, 40.400189, -75.917622)) %>%
  mutate(x = as.numeric(x), y = as.numeric(y))

} else{
  states_centers2 <- states_centers %>%
    #remove the invaded states
    filter(!state %in% c("Delaware", "Maryland", "New Jersey", "New York", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "West Virginia")) %>%
    #remove the others in the dataset
    filter(!is.na(x), !is.na(y)) %>%
    rbind(c("SLF", NA, 40.400189, -75.917622)) %>%
    mutate(x = as.numeric(x), y = as.numeric(y))
  
}

colnames(states_centers2)[1] <- "geopol_unit"

#########
#read in the mean downsampled rasters
########

#USA
enm_data_usa_mean <- raster("./data/geotiff_enms/slftoh_usa_downsampled_x4_mean.tif")

#plot downsampled rasters
#fortify (dataframe-fy) the global model
enm_usa_df <- fortify(enm_data_usa_mean, maxpixels = 1e10)
colnames(enm_usa_df)[3] <- "value"
#remove the NA values to shrink the object size
enm_usa_df <- enm_usa_df[!is.na(enm_usa_df$value),]

#round the value points
enm_usa_df$value <- round(enm_usa_df$value, digits = 2)

#need to read in trade data to shape network in map figure
if(present_switch == TRUE){
  states_trade_value <- read_csv(file = "./data/trade/summary_states_trade_value_v2.csv", col_names = T)
  states_trade_mass <- read_csv(file = "./data/trade/summary_states_trade_mass_v2.csv", col_names = T)
  states_trade <- left_join(states_trade_value, states_trade_mass, by = c("destination", "status"), suffix = c("_trade", "_mass"))
  
} else {
  #need to add the revised version of trade here with new "FUTURE STATES"
  states_trade_value <- read_csv(file = "./data/trade/FINAL_DATA/summary_states_trade_value_future_v2.csv", col_names = T)
  states_trade_mass <- read_csv(file = "./data/trade/FINAL_DATA/summary_states_trade_mass_future_v2.csv", col_names = T)
  states_trade <- left_join(states_trade_value, states_trade_mass, by = c("destination", "status"), suffix = c("_trade", "_mass"))
}

#now add the "invaded core" == SLF as a column

states_trade2 <- states_trade %>%
  cbind("SLF", .) %>%
  as_tibble() %>%
  mutate(origin = `"SLF"`) %>%
  dplyr::select(-`"SLF"`) %>%
  dplyr::select(origin, everything())

#now merge the coords for destinations (ONLY WINE PRODUCING STATES)
states_trade_final <- left_join(states_trade2, states_centers2, by = c("destination" = "geopol_unit")) %>%
  filter(!is.na(x), !is.na(y))

#add the coords for SLF as a separate set of cols
states_trade_final <- states_centers2 %>%
  filter(geopol_unit == "SLF") %>%
  dplyr::select(x,y) %>%
  mutate(xend = x, yend = y) %>%
  dplyr::select(xend, yend) %>%
  cbind(states_trade_final, .) %>%
  as_tibble()

#figure out the top 25 trading partners that grow winegrapes
top25states <- states_trade_final %>%
  arrange(desc(avg_infected_mass)) %>%
  .[["destination"]] %>%
  .[1:25]
#top 10
top10states <- states_trade_final %>%
  arrange(desc(avg_infected_mass)) %>%
  .[["destination"]] %>%
  .[1:10]

#check that top 10 trade partners are also wine producers too
#do the present version
if(present_switch == TRUE){
  states_trade %>%
    filter(!destination %in% c("Delaware", "Maryland", "New Jersey", "Pennsylvania", "Virginia", "District of Columbia")) %>%
    arrange(desc(avg_infected_mass)) %>%
    .[["destination"]] %>%
    .[1:10]
  
} else{
  states_trade %>%
    filter(!destination %in% c("Delaware", "Maryland", "New Jersey", "New York", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "West Virginia", "District of Columbia")) %>%
    arrange(desc(avg_infected_mass)) %>%
    .[["destination"]] %>%
    .[1:10]
  
}

#the top ten pass, the top 25 get stuck at 22 
#(Rhode island, Iowa, Maine, Louisiana vs wine: Iowa, Maine, Louisiana, Washinton)
#FIXED: with commenting in the the state capitols version of centers!



#now let's try just the straight ggplot2 network approach?

#plot just the states and suitability first
map_plot <- ggplot() +
  geom_raster(data = enm_usa_df, aes(x = x, y = y, fill = rescale(value)), alpha=0.9, show.legend = T) +
  #geom_raster(data = enm_usa_df, aes(x = x, y = y, fill = value), alpha=0.8, show.legend = T) +
    #scale_fill_viridis(limits= c(0,1), name = "Suitability", option = "cividis", begin = 0.35) +
    #scale_fill_gradient(limits= c(0,1), name = "", low = "#FFFFFF", high = "#000000") +
    #scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#b0513e","#d99d50", "#FFFFFF"))) +
  scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#e31a1c","#fd8d3c", "#fecc5c", "#FFFFFF"))) +
  geom_polygon(data = map_data('state'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.10) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_quickmap(xlim = c(-124.7628, -66.94889), ylim = c(24.52042, 49.3833)) +
  ggtitle(label = "")

#add in lines for the infected states
if(present_switch == TRUE){
  map_plot <- map_plot +
    geom_polygon(data = map_data('state', c("Delaware", "Maryland", "New Jersey", "Pennsylvania", "Virginia")), aes(x = long, y = lat, group = group), fill = NA, color = "#e31a1c", lwd = 0.75)
  
} else{
  map_plot <- map_plot +
    geom_polygon(data = map_data('state', c("Delaware", "Maryland", "New Jersey", "New York", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "West Virginia")), aes(x = long, y = lat, group = group), fill = NA, color = "#e31a1c", lwd = 0.75)
}

#add in the wine AVA data
map_plot <- map_plot +
    geom_point(data = usa_wineries, aes(x = x, y = y), color = "black", fill = "purple", size = 1.5, shape = 21, alpha = 0.99, show.legend = F)
  #geom_point(data = states_centers2 %>% filter(geopol_unit %in% top25states), aes(x = x, y = y), color = "black", fill = "white", shape = 21, stroke = 0.5, size = 2.5) +
  #geom_point(data = states_centers2 %>% filter(geopol_unit == "SLF"), aes(x = x, y = y), color = "black", fill = "red", shape = 21, stroke = 0.5, size = 3)


#add in the trade
map_plot <- map_plot +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend), data = states_trade_final %>% filter(destination %in% top10states), size = 1, curvature = 0.33, alpha = 0.8, show.legend = T, color = "#778899",
               arrow = arrow(ends = "first", length = unit(0.01, "npc"), type = "closed")) #+
  #geom_curve(aes(x = x, y = y, xend = xend, yend = yend, size = (avg_infected_mass)), 
   #          data = states_trade_final %>% filter(destination %in% top10states), 
    #         curvature = 0.33, alpha = 0.8, show.legend = T, color = "#778899",
     #        arrow = arrow(ends = "first", length = unit(0.01, "npc"), type = "closed")) +
    #geom_curve(aes(x = x, y = y, xend = xend, yend = yend, size = (avg_infected_mass)), data = states_trade_final, curvature = 0.33, alpha = 0.5, show.legend = F) +
    #scale_size_continuous(range = c(0.5, 5), name = "Interstate Exports, MT", limits = c(5e6, 1e9))

#show it
map_plot

ggsave(plot = map_plot, filename = "./submission/Global_MAXENT/figures/Fig 1/states_mean_model_map_v2_9_1.5.pdf", width = 7.5, dpi = 300)

ggsave(plot = map_plot, filename = "./submission/Global_MAXENT/figures/Fig 1/states_mean_model_map_v2_9_1.5.tif", dpi = 300, device = "tiff")

################################################
#Countries
################################################

enm_data_mean <- raster("./data/geotiff_enms/slftoh_downsampled_x4_mean.tif")

#read in the centroids for a check
countries_centers <- read_csv(file = "./data/wineries/geopolitical_centers_world.csv") %>%
  dplyr::rename(y = y_coord, x = x_coord)

#edit the world centers to also have the SLF US invasion point
countries_centers2 <- countries_centers %>%
  #remove the others in the dataset
  filter(!is.na(x), !is.na(y)) %>%
  #add center for SLF invasion in US
  rbind(c("SLF", NA, -75.917622, 40.400189)) %>%
  #add the netherlands, not a wine country but needs a set of coords
  rbind(c("Netherlands", NA, unlist(gCentroid(SpatialPointsDataFrame(map_data('world')[map_data('world')$region %in% "Netherlands",1:2], data = map_data('world')[map_data('world')$region %in% "Netherlands",]))@coords))) %>%
  #same for indonesia
  rbind(c("Indonesia", NA, unlist(gCentroid(SpatialPointsDataFrame(map_data('world')[map_data('world')$region %in% "Indonesia",1:2], data = map_data('world')[map_data('world')$region %in% "Indonesia",]))@coords))) %>%
  #same for these if going for top 25: Singapore, Taiwan, Saudi Arabia, Vietnam, Venezuela, Gibraltar
  mutate(x = as.numeric(x), y = as.numeric(y))

colnames(countries_centers2)[1] <- "geopol_unit"

#read in the world trade data
#need to add the revised version of trade here with new "FUTURE STATES"
if(present_switch == TRUE){
  countries_trade_value <- read_csv(file = "./data/trade/summary_countries_trade_value.csv", col_names = T)
  countries_trade_mass <- read_csv(file = "./data/trade/summary_countries_trade_mass.csv", col_names = T)
  countries_trade <- left_join(countries_trade_value, countries_trade_mass, by = c("destination"), suffix = c("_trade", "_mass"))
} else{
  countries_trade_value <- read_csv(file = "./data/trade/FINAL_DATA/summary_countries_trade_value_future.csv", col_names = T)
  countries_trade_mass <- read_csv(file = "./data/trade/FINAL_DATA/summary_countries_trade_mass_future.csv", col_names = T)
  countries_trade <- left_join(countries_trade_value, countries_trade_mass, by = c("destination"), suffix = c("_trade", "_mass"))
  
}


countries_trade2 <- countries_trade %>%
  filter(!destination %in% c("China", "Japan", "Korea, South", "India")) %>%
  cbind("SLF", .) %>%
  as_tibble() %>%
  mutate(origin = `"SLF"`) %>%
  dplyr::select(-`"SLF"`) %>%
  dplyr::select(origin, everything())

#now merge the coords for destinations (ONLY WINE PRODUCING countries)
countries_trade_final <- left_join(countries_trade2, countries_centers2, by = c("destination" = "geopol_unit")) %>%
  filter(!is.na(x), !is.na(y))

#add the coords for SLF as a separate set of cols
countries_trade_final <- countries_centers2 %>%
  filter(geopol_unit == "SLF") %>%
  dplyr::select(x,y) %>%
  mutate(xend = x, yend = y) %>%
  dplyr::select(xend, yend) %>%
  cbind(countries_trade_final, .) %>%
  as_tibble()

#figure out the top 25 trading partners that grow winegrapes
top25countries <- countries_trade_final %>%
  arrange(desc(avg_infected_mass)) %>%
  .[["destination"]] %>%
  .[1:25]
#top 10
top10countries <- countries_trade_final %>%
  arrange(desc(avg_infected_mass)) %>%
  .[["destination"]] %>%
  .[1:10]

#confirm that the top 10 countries are actually wine producers
countries_trade2 %>%
  arrange(desc(avg_infected_mass)) %>%
  .[["destination"]] %>%
  .[1:10]


#presence data?
slf_coords <- read_csv("./data/Spotted Lanternfly/MAXENT_INPUT_VARIATE/SLF_FINAL_withPDA_24Sept18_v4_8.csv") %>%
  .[,2:3] %>%
  mutate(longitude = Longitude, latitude = Latitude) %>%
  dplyr::select(longitude, latitude)
colnames(slf_coords) <- c("x", "y")


#plot downsampled rasters
#fortify (dataframe-fy) the global model
enm_df <- fortify(enm_data_mean, maxpixels = 1e10)
colnames(enm_df)[3] <- "value"
#remove the NA values to shrink the object size
enm_df <- enm_df[!is.na(enm_df$value),]



#Plot sequentially to make it easier to look at parts

#plot just the countries and suitability first
map_plot_countries <- ggplot() +
  geom_raster(data = enm_df, aes(x = x, y = y, fill = rescale(value)), alpha=0.9, show.legend = T) +
  #scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#b0513e","#d99d50", "#f5e3ad"))) +
  #scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#b0513e","#d99d50", "#FFFFFF"))) +
  scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#e31a1c","#fd8d3c", "#fecc5c", "#FFFFFF"))) +
  #scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#b0513e","#d99d50", "#e3c62a", "#f5e3ad"))) +
  #scale_fill_viridis(limits= c(0,1), name = "Suitability", option = "cividis", begin = 0.35) +
  geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.15) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_quickmap(xlim = c(-164.5, 163.5), ylim = c(-55,85)) +
  ggtitle(label = "")
  
#add in lines for the infected states
if(present_switch == TRUE){
  map_plot_countries <- map_plot_countries +
    geom_polygon(data = map_data('state', c("Delaware", "Maryland", "New Jersey", "Pennsylvania", "Virginia")), aes(x = long, y = lat, group = group), fill = NA, color = "red", lwd = 0.5)
} else{
  map_plot_countries <- map_plot_countries +
    geom_polygon(data = map_data('state', c("Delaware", "Maryland", "New Jersey", "New York", "North Carolina", "Ohio", "Pennsylvania", "Virginia", "West Virginia")), aes(x = long, y = lat, group = group), fill = NA, color = "#e31a1c", lwd = 0.75)
}
  

#add in the wine AVA data (global equivalent)
map_plot_countries <- map_plot_countries +
  geom_point(data = wineries, aes(x = x, y = y), color = "black", fill = "purple", size = 0.5, shape = 21, alpha = 0.99, stroke = 0.2, show.legend = F)

#add in the trade
map_plot_countries <- map_plot_countries +
  #eom_curve(aes(x = x, y = y, xend = xend, yend = yend, size = (avg_infected_mass)), 
  #           data = countries_trade_final %>% filter(destination %in% top10countries), 
  #           curvature = 0.33, alpha = 0.8, show.legend = T, color = "#778899", #old color:#5273f7 
  #           arrow = arrow(ends = "first", length = unit(0.01, "npc"), type = "closed")) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend), data = countries_trade_final %>% filter(destination %in% top10countries), size = 0.45, curvature = 0.33, alpha = 0.8, show.legend = T, color = "#778899", arrow = arrow(ends = "first", length = unit(0.01, "npc"), type = "closed")) 
  #scale_size_continuous(range = c(0.5, 5), name = "Trade with \nInfected States", limits = c(1e9, 5e10)) +
  #scale_size_continuous(range = c(0.5, 5), name = "Global Exports, MT", limits = c(5e7, 5e10))

#print it
map_plot_countries 

ggsave(plot = map_plot_countries, filename = "./submission/Global_MAXENT/figures/Fig 1/world_mean_model_map_v2_9_0.5.pdf", width = 7.5, dpi = 300)

ggsave(plot = map_plot_countries, filename = "./submission/Global_MAXENT/figures/Fig 1/world_mean_model_map_v2_9_0.5.tif", dpi = 300, device = "tiff")


#with SLF coords
map_plot_countries +
  geom_point(data = slf_coords, aes(x = x, y = y), shape = 17)


#########################################################################################################

enm_toh2 <- raster("./data/geotiff_enms/toh_usa.tif")

#read in toh points
toh_coords1 <- read_csv("./data/ailanthus/toh_gbif_05-22-2019.csv") %>%
  .[,2:3] %>%
  dplyr::select(longitude, latitude)
colnames(toh_coords1) <- c("x", "y")

#EDDMaps data
toh_coords2 <- read_csv("./data/ailanthus/EDDMapS_19747.csv", trim_ws = T, skip = 3) %>%
  dplyr::select(Longitude, Latitude) %>%
  filter(!is.na(Longitude) | !is.na(Latitude))
colnames(toh_coords2) <- c("x", "y")



coordinates(toh_coords1) <-  ~ x + y
crs(toh_coords1) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
coordinates(toh_coords2) <-  ~ x + y
crs(toh_coords2) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'


#crop the layer to continental US
enm_toh2 <- raster::crop(enm_toh2, extent(-124.7628, -66.94889, 24.52042, 49.3833))

enm_toh3 <- as.data.frame(as(enm_toh2, "SpatialPixelsDataFrame"))
colnames(enm_toh3) <- c("value", "x", "y")

#trim the toh coords
states <- readOGR(dsn = "./data/environment/gadm36_USA_shp/gadm36_USA_1.shp", verbose = T)
crs(toh_coords1) <- crs(states)
toh_coords1 <- toh_coords1[!is.na(sp::over(toh_coords1, as(states, "SpatialPolygons")))]
toh_coords1 <- toh_coords1 %>%
  crop(., extent(-124.7628, -66.94889, 24.52042, 49.3833))

crs(toh_coords2) <- crs(states)
toh_coords2 <- toh_coords2[!is.na(sp::over(toh_coords2, as(states, "SpatialPolygons")))]
toh_coords2 <- toh_coords2 %>%
  crop(., extent(-124.7628, -66.94889, 24.52042, 49.3833))

#GBIF
nrow(toh_coords1@coords)
unique(toh_coords1@coords) %>% nrow(.)
#EDD
nrow(toh_coords2@coords)
unique(toh_coords2@coords) %>% nrow(.)

intersect(unique(paste0(toh_coords1@coords)), unique(paste0(toh_coords2@coords)))

ggplot() +
  geom_polygon(data = map_data('usa'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.25) +
  geom_polygon(data = map_data('state'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.10) +
  #geom_raster(data = enm_toh3, aes(x = x, y = y, fill = value), alpha = 0.8, show.legend = T) +
  geom_point(data = as.data.frame(toh_coords1), aes(x = x, y = y), shape = 16) +
  #geom_raster(data = toh4, aes(x = x, y = y, fill = value), alpha = 0.8, show.legend = T) +
  #scale_fill_viridis(limits = c(0,1)) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#plot of where SLF is found
slf_coords

ggplot() +
  geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group), fill = "grey", color = "black", lwd = 0.15) +
  geom_point(data = slf_coords, aes(x = x, y = y), shape = 17, size = 1.75, color = "blue") +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_quickmap(xlim = c(-164.5, 163.5), ylim = c(-55,85)) +
  ggtitle(label = "")











#get the individual models plotted

#USA

enm_data_usa_slf <- raster("./data/geotiff_enms/slf_downsampled_x4.tif")
enm_data_usa_slftoh <- raster("./data/geotiff_enms/slftoh_downsampled_x4.tif")
enm_data_usa_toh <- raster("./data/geotiff_enms/toh_downsampled_x4.tif")

#do the same stuff for all three individual models
#fortify (dataframe-fy) the global model
enm_usa_df_slftoh <- fortify(enm_data_usa_slftoh, maxpixels = 1e10)
colnames(enm_usa_df_slftoh)[3] <- "value"
enm_usa_df_slf <- fortify(enm_data_usa_slf, maxpixels = 1e10)
colnames(enm_usa_df_slf)[3] <- "value"
enm_usa_df_toh <- fortify(enm_data_usa_toh, maxpixels = 1e10)
colnames(enm_usa_df_toh)[3] <- "value"

#remove the NA values to shrink the object size
enm_usa_df_slftoh <- enm_usa_df_slftoh[!is.na(enm_usa_df_slftoh$value),]
enm_usa_df_slf <- enm_usa_df_slf[!is.na(enm_usa_df_slf$value),]
enm_usa_df_toh <- enm_usa_df_toh[!is.na(enm_usa_df_toh$value),]

#round the value points
enm_usa_df_slftoh$value <- round(enm_usa_df_slftoh$value, digits = 2)
enm_usa_df_slf$value <- round(enm_usa_df_slf$value, digits = 2)
enm_usa_df_toh$value <- round(enm_usa_df_toh$value, digits = 2)


#now plots
ggplot() +
  geom_raster(data = enm_usa_df_slftoh, aes(x = x, y = y, fill = rescale(value)), alpha=0.9, show.legend = F) +
  scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#e31a1c","#fd8d3c", "#fecc5c", "#FFFFFF"))) +
  geom_polygon(data = map_data('state'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.10) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_quickmap(xlim = c(-124.7628, -66.94889), ylim = c(24.52042, 49.3833)) +
  ggtitle(label = "SLFTOH")

ggplot() +
  geom_raster(data = enm_usa_df_slf, aes(x = x, y = y, fill = rescale(value)), alpha=0.9, show.legend = F) +
  scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#e31a1c","#fd8d3c", "#fecc5c", "#FFFFFF"))) +
  geom_polygon(data = map_data('state'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.10) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_quickmap(xlim = c(-124.7628, -66.94889), ylim = c(24.52042, 49.3833)) +
  ggtitle(label = "SLF")

ggplot() +
  geom_raster(data = enm_usa_df_toh, aes(x = x, y = y, fill = rescale(value)), alpha=0.9, show.legend = F) +
  scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#e31a1c","#fd8d3c", "#fecc5c", "#FFFFFF"))) +
  geom_polygon(data = map_data('state'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.10) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_quickmap(xlim = c(-124.7628, -66.94889), ylim = c(24.52042, 49.3833)) +
  ggtitle(label = "TOH")

ggplot() +
  geom_raster(data = enm_usa_df, aes(x = x, y = y, fill = rescale(value)), alpha=0.9, show.legend = F) +
  scale_fill_gradientn(limits= c(0,1), name = "Suitability", colors = rev(c("#e31a1c","#fd8d3c", "#fecc5c", "#FFFFFF"))) +
  geom_polygon(data = map_data('state'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.10) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_quickmap(xlim = c(-124.7628, -66.94889), ylim = c(24.52042, 49.3833)) +
  ggtitle(label = "Mean")




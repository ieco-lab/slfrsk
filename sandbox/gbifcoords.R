library(tidyverse) #data tidying tools
library(spocc) #query gbif and format as a dataframe
library(scrubr) #clean records for gbif data
library(here) #make paths play nicely
library(humboldt) #rarefy points
library(tcltk) #humboldt progress bar


#query for both species from gbif with limit of queries to 10^5
slf_gbif <- occ(query = 'Lycorma delicatula', from = 'gbif', limit = 1e5, has_coords = TRUE, throw_warnings = TRUE)
toh_gbif <- occ(query = 'Ailanthus altissima', from = 'gbif', limit = 1e5, has_coords = TRUE, throw_warnings = TRUE)

#tibble the raw queries for saving
slf_gbif_final <- as_tibble(slf_gbif$gbif$data$Lycorma_delicatula)
toh_gbif_final <- as_tibble(toh_gbif$gbif$data$Ailanthus_altissima)

toh_gbif_final$networkKeys <- NULL

#save the raw queries with the date stamp for current date
write_csv(x = slf_gbif_final, file = file.path(here(), "data-raw", paste0( "slf_gbif_", format(Sys.Date(), "%Y-%d-%m"), ".csv")))
write_csv(x = toh_gbif_final, file = file.path(here(), "data-raw", paste0( "toh_gbif_", format(Sys.Date(), "%Y-%d-%m"), ".csv")))


#isolate the coords
slf_coords <- occ2df(slf_gbif)
toh_coords <- occ2df(toh_gbif)

#***************************#
#if we add the extra data, we would merge here
#***************************#

#check the taxonomy for only correct queries
unique(slf_coords$name) #all the same
unique(toh_coords$name)

#toh data contains a bunch of synonyms according to GBIF (https://www.gbif.org/species/3190653)
#therefore, we are going to make them all the same, the top species designation
toh_coords$name <- "Ailanthus altissima (Mill.) Swingle"

#now use scrubr to clean the coords
#slf
slf_coords <- slf_coords %>%
  coord_incomplete() %>%  #rm incomplete coords, those that lack valid lat and long
  coord_impossible() %>% #rm impossible coords, those that are not possible (e.g., lat > 90)
  coord_unlikely() %>%  #rm unlikely coords, such as those at 0,0
  dedup(how = "one", tolerance = 0.99) #rm duplicate coords

#toh
toh_coords <- toh_coords %>%
  coord_incomplete() %>%  #rm incomplete coords, those that lack valid lat and long
  coord_impossible() %>% #rm impossible coords, those that are not possible (e.g., lat > 90)
  coord_unlikely() #%>%  #rm unlikely coords, such as those at 0,0
  #dedup(how = "one", tolerance = 0.95) #rm duplicate coords


#rarefy points
slf_coords2 <- humboldt.occ.rarefy(in.pts = slf_coords, colxy = 2:3, rarefy.dist = 10, rarefy.units = "km", run.silent.rar = F)
toh_coords2 <- humboldt.occ.rarefy(in.pts = toh_coords, colxy = 2:3, rarefy.dist = 10, rarefy.units = "km", run.silent.rar = F)

toh_coords3 <- toh_coords2 %>%
  dedup(how = "one", tolerance = 0.99)

#plot SLF points to check and rm errant points
ggplot() +
  geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.15) +
  geom_point(data = slf_coords, aes(x = longitude, y = latitude), color = "red", size = 2) +
  geom_point(data = slf_coords2, aes(x = longitude, y = latitude), color = "blue", shape = 2) +
  coord_quickmap(xlim = c(-164.5, 163.5), ylim = c(-55,85)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()

#we have points in: MA, NE, OR, DE coast for SLF that need to be removed
#key should work to get rid of identified points
slf_coords3 <- slf_coords2 %>%
  filter(key != "2860187641") %>% #rm OR---lat, lon:(43.63691, -121.85569)
  filter(key != "2862292948") %>% #rm NE---lat, lon:(42.50641,-101.01562)
  filter(key != "2864687343") %>% #rm DE---lat, lon:(37.91855, -75.14999)
  filter(!key %in% c("2856537682", "2851117559")) #rm MA---lat, lon: (42.20994, -71.18331)
#SLF
ggplot() +
  geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.15) +
  geom_point(data = slf_coords, aes(x = longitude, y = latitude), color = "red", size = 2) +
  geom_point(data = slf_coords3, aes(x = longitude, y = latitude), color = "blue", shape = 2) +
  coord_quickmap(xlim = c(-164.5, 163.5), ylim = c(-55,85)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()

#TOH
ggplot() +
  geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group), fill = NA, color = "black", lwd = 0.15) +
  geom_point(data = toh_coords, aes(x = longitude, y = latitude), color = "red", size = 2) +
  geom_point(data = toh_coords3, aes(x = longitude, y = latitude), color = "blue", shape = 2) +
  coord_quickmap(xlim = c(-164.5, 163.5), ylim = c(-55,85)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme_bw()

#change order of final coords to be lat, lon
slf_coords3 <- slf_coords3 %>%
  dplyr::select(name, latitude, longitude, prov, date, key)
toh_coords3 <- toh_coords3 %>%
  dplyr::select(name, latitude, longitude, prov, date, key)


write_csv(slf_coords3, file = file.path(here(), "data-raw", "slf_gbif_cleaned_coords_2020.csv"))
write_csv(toh_coords3, file = file.path(here(), "data-raw", "toh_gbif_cleaned_coords_2020.csv"))

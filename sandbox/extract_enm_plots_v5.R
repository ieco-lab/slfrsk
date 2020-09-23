#plot extracted ENM data
#authors: NAH

#read in required packages
library(tidyverse)

#set local dir
setwd("/Users/nicholashuron/Google Drive File Stream/My Drive/spotted_lanternfly_ieco_projects/")

#########################################################################################################
#states
#read in the data
slf_extract <- read_csv(file = "./data/slf_enm_extract/extract_states_02_15_19_maxent_slf+atc-bio02.csv", col_names = T)
toh_extract <- read_csv(file = "./data/slf_enm_extract/extract_states_10_29_18_maxent_toh+atc-bio02.csv", col_names = T)
slftoh_extract <- read_csv(file = "./data/slf_enm_extract/extract_states_11_07_18_maxent_slf+toh+atc-bio02.csv", col_names = T)
jung_extract <- read_csv(file = "./data/slf_enm_extract/extract_states_jung.csv", col_names = T)

#rescale jung model extracts by dividing by 100
jung_extract[,c(-1,-6)] <- jung_extract[,c(-1,-6)] / 100

#instead, can add model as a categorical var
slf_extract <- slf_extract %>%
  mutate(model = "slf")
toh_extract <- toh_extract %>%
  mutate(model = "toh")
slftoh_extract <- slftoh_extract %>%
  mutate(model = "slftoh")
jung_extract <- jung_extract %>%
  mutate(model = "jung")

#rename columns
colnames(slf_extract) <- gsub("max", replacement = "obs_max", x = colnames(slf_extract))
colnames(slf_extract) <- gsub("min", replacement = "obs_min", x = colnames(slf_extract))
colnames(slf_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(slf_extract))
colnames(slf_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(slf_extract))

colnames(toh_extract) <- gsub("max", replacement = "obs_max", x = colnames(toh_extract))
colnames(toh_extract) <- gsub("min", replacement = "obs_min", x = colnames(toh_extract))
colnames(toh_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(toh_extract))
colnames(toh_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(toh_extract))

colnames(slftoh_extract) <- gsub("max", replacement = "obs_max", x = colnames(slftoh_extract))
colnames(slftoh_extract) <- gsub("min", replacement = "obs_min", x = colnames(slftoh_extract))
colnames(slftoh_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(slftoh_extract))
colnames(slftoh_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(slftoh_extract))

colnames(jung_extract) <- gsub("max", replacement = "obs_max", x = colnames(jung_extract))
colnames(jung_extract) <- gsub("min", replacement = "obs_min", x = colnames(jung_extract))
colnames(jung_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(jung_extract))
colnames(jung_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(jung_extract))

#aggregate/join models
extracts <- bind_rows(slf_extract, toh_extract, slftoh_extract) %>%
  filter(model != "jung") %>%
  group_by(geopol_unit) %>%
  arrange(geopol_unit)
#all four models version
extracts4 <- bind_rows(slf_extract, toh_extract, slftoh_extract, jung_extract) %>%
  group_by(geopol_unit) %>%
  arrange(geopol_unit)

#state id's for adding to extracts
stateid <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

#add the id's to extracts
extracts <- bind_cols(extracts, tibble(ID = rep(stateid, each = length(unique(extracts$model))))) %>%
  mutate(status = ifelse(ID %in% c("DE", "MD", "NJ","PA", "VA"), "established", "not established"))

extracts4 <- bind_cols(extracts4, tibble(ID = rep(stateid, each = length(unique(extracts4$model))))) %>%
  mutate(status = ifelse(ID %in% c("DE", "MD", "NJ","PA", "VA"), "established", "not established"))


#can bind states and stateid if we use extracts once it has the two in it
states <- extracts %>%
  dplyr::select(geopol_unit, ID) %>%
  distinct()

  #########################################################################################################
#plot all of the points for each model and do true mean and SE across them

#get MEAN AND SE across models
summary_extracts4 <- extracts4 %>%
  group_by(geopol_unit, ID, status) %>%
  summarize(grand_mean_max = mean(obs_max), grand_se_max = sd(obs_max) / sqrt(n_distinct(model)))

#3 model version
summary_extracts <- extracts %>%
  group_by(geopol_unit, ID, status) %>%
  summarize(grand_mean_max = mean(obs_max), grand_se_max = sd(obs_max) / sqrt(n_distinct(model)))


#plot results
  #4 models
ggplot(data = summary_extracts4,aes(y = reorder(ID, grand_mean_max))) +
  geom_point(data = left_join(extracts4, summary_extracts4), aes(x = obs_max, color = model), size = 1.5) +
  geom_errorbarh(aes(xmin = grand_mean_max - grand_se_max, xmax = grand_mean_max + grand_se_max, color = status)) +
  geom_point(aes(x = grand_mean_max, color = status), shape = 18, size = 2) +
  labs(x = "Observed Suitability", y = "") +
  lims(x = c(0,1)) +
  ggtitle(label = "Establishment Potential (max)") +
  theme(axis.text.y = element_text(hjust = 1, color = summary_extracts4 %>%
                                     group_by(ID, status) %>%
                                     distinct(grand_mean_max) %>%
                                     mutate(infcolor = ifelse(status == "established", "#ff0000", "#000000")) %>% 
                                     arrange((grand_mean_max)) %>%
                                     dplyr::select(infcolor) %>%
                                     .$infcolor)) +
  scale_colour_manual(name = "", values = c("jung" = "#C77CFF", "slftoh" = "#00BFC4", "toh" = "#F8766D", "slf" = "#7CAE00", "established" = "#ff0000", "not established" = "#000000"), breaks = c("established", "not established", "jung", "slftoh", "toh", "slf"))

  #3 models
ggplot(data = summary_extracts, aes(y = reorder(ID, grand_mean_max))) +
  geom_point(data = left_join(extracts, summary_extracts), aes(x = obs_max, color = model), size = 1.5) +
  geom_errorbarh(aes(xmin = grand_mean_max - grand_se_max, xmax = grand_mean_max + grand_se_max, color = status)) +
  geom_point(aes(x = grand_mean_max, color = status), shape = 18, size = 2) +
  labs(x = "Observed Suitability", y = "") +
  lims(x = c(0,1)) +
  ggtitle(label = "Establishment Potential (max)") +
  theme(axis.text.y = element_text(hjust = 1, color = summary_extracts %>%
                                     group_by(ID, status) %>%
                                     distinct(grand_mean_max) %>%
                                     mutate(infcolor = ifelse(status == "established", "#ff0000", "#000000")) %>% 
                                     arrange((grand_mean_max)) %>%
                                     dplyr::select(infcolor) %>%
                                     .$infcolor)) +
  scale_colour_manual(name = "", values = c("jung" = "#C77CFF", "slftoh" = "#00BFC4", "toh" = "#F8766D", "slf" = "#7CAE00", "established" = "#ff0000", "not established" = "#000000"), breaks = c("established", "not established", "jung", "slftoh", "toh", "slf"))

#########################################################################################################
#countries
#read in the data for all four models
countries_slf_extract <- read_csv(file = "./data/slf_enm_extract/extract_world_11_07_18_maxent_slf+atc-bio02.csv", col_names = T)
countries_toh_extract <- read_csv(file = "./data/slf_enm_extract/extract_world_11_07_18_maxent_toh+atc-bio02.csv", col_names = T)
countries_slftoh_extract <- read_csv(file = "./data/slf_enm_extract/extract_world_11_07_18_maxent_slf+toh+atc-bio02.csv", col_names = T)
countries_jung_extract <- read_csv(file = "./data/slf_enm_extract/extract_world_jung.csv", col_names = T)

countries_jung_extract[,c(-1,-6)] <- countries_jung_extract[,c(-1,-6)] / 100

#instead, can add model as a categorical var
countries_slf_extract <- countries_slf_extract %>%
  mutate(model = "slf")
countries_toh_extract <- countries_toh_extract %>%
  mutate(model = "toh")
countries_slftoh_extract <- countries_slftoh_extract %>%
  mutate(model = "slftoh")
countries_jung_extract <- countries_jung_extract %>%
  mutate(model = "jung")

#all rows with NA appear to also have the other issues, so we can just use drop_na()
countries_slf_extract <- drop_na(countries_slf_extract)
countries_toh_extract <- drop_na(countries_toh_extract)
countries_slftoh_extract <- drop_na(countries_slftoh_extract)
countries_jung_extract <- drop_na(countries_jung_extract)

#clean up the extract data
colnames(countries_slf_extract) <- gsub("max", replacement = "obs_max", x = colnames(countries_slf_extract))
colnames(countries_slf_extract) <- gsub("min", replacement = "obs_min", x = colnames(countries_slf_extract))
colnames(countries_slf_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(countries_slf_extract))
colnames(countries_slf_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(countries_slf_extract))
#clean up the extract data
colnames(countries_toh_extract) <- gsub("max", replacement = "obs_max", x = colnames(countries_toh_extract))
colnames(countries_toh_extract) <- gsub("min", replacement = "obs_min", x = colnames(countries_toh_extract))
colnames(countries_toh_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(countries_toh_extract))
colnames(countries_toh_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(countries_toh_extract))
#clean up the extract data
colnames(countries_slftoh_extract) <- gsub("max", replacement = "obs_max", x = colnames(countries_slftoh_extract))
colnames(countries_slftoh_extract) <- gsub("min", replacement = "obs_min", x = colnames(countries_slftoh_extract))
colnames(countries_slftoh_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(countries_slftoh_extract))
colnames(countries_slftoh_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(countries_slftoh_extract))

colnames(countries_jung_extract) <- gsub("max", replacement = "obs_max", x = colnames(countries_jung_extract))
colnames(countries_jung_extract) <- gsub("min", replacement = "obs_min", x = colnames(countries_jung_extract))
colnames(countries_jung_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(countries_jung_extract))
colnames(countries_jung_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(countries_jung_extract))


#aggregate/join models
  #3 MODEL VERSION
gextracts <- bind_rows(countries_slf_extract, countries_toh_extract, countries_slftoh_extract, countries_jung_extract) %>%
  filter(model != "jung") %>%
  group_by(geopol_unit) %>%
  arrange(geopol_unit)

  #4 MODEL VERSION
gextracts4 <- bind_rows(countries_slf_extract, countries_toh_extract, countries_slftoh_extract, countries_jung_extract) %>%
  group_by(geopol_unit) %>%
  arrange(geopol_unit)

#get MEAN AND SE across models
  #3 models
summary_gextracts <- gextracts %>%
  group_by(geopol_unit) %>%
  summarize(grand_mean_max = mean(obs_max), grand_se_max = sd(obs_max) / sqrt(n_distinct(model)))
  #4 models
summary_gextracts4 <- gextracts4 %>%
  group_by(geopol_unit) %>%
  summarize(grand_mean_max = mean(obs_max), grand_se_max = sd(obs_max) / sqrt(n_distinct(model)))

#max
#select top 50 countries with highest max suitability
  #3 models
top_countries <- summary_gextracts %>%
  group_by(geopol_unit) %>%
  summarize(grand_mean_max) %>%
  arrange(desc(grand_mean_max)) %>%
  .[1:50,] %>%
  dplyr::select(geopol_unit) %>%
  unlist(.)
top_countries <- unname(top_countries)
  
  #4 models
top_countries4 <- summary_gextracts4 %>%
  group_by(geopol_unit) %>%
  summarize(grand_mean_max) %>%
  arrange(desc(grand_mean_max)) %>%
  .[1:50,] %>%
  dplyr::select(geopol_unit) %>%
  unlist(.)
top_countries4 <- unname(top_countries4)


#3 model figure
summary_gextracts %>%
  filter(geopol_unit %in% top_countries) %>%
ggplot(data = ., aes(y = reorder(geopol_unit, grand_mean_max))) +
  geom_point(data = gextracts %>% filter(geopol_unit %in% top_countries) %>% left_join(., filter(summary_gextracts, geopol_unit %in% top_countries)), aes(x = obs_max, color = model), size = 1.5) +
  geom_errorbarh(aes(xmin = grand_mean_max - grand_se_max, xmax = grand_mean_max + grand_se_max)) +
  geom_point(aes(x = grand_mean_max), shape = 18, size = 2) +
  labs(x = "Observed Suitability", y = "") +
  lims(x = c(0,1)) +
  ggtitle(label = "Establishment Potential (max)") +
  scale_colour_manual(name = "", values = c("jung" = "#C77CFF", "slftoh" = "#00BFC4", "toh" = "#F8766D", "slf" = "#7CAE00"), breaks = c("jung", "slftoh", "toh", "slf"))

#4 model figure
summary_gextracts4 %>%
  filter(geopol_unit %in% top_countries4) %>%
  ggplot(data = ., aes(y = reorder(geopol_unit, grand_mean_max))) +
  geom_point(data = gextracts4 %>% filter(geopol_unit %in% top_countries4) %>% left_join(., filter(summary_gextracts4, geopol_unit %in% top_countries4)), aes(x = obs_max, color = model), size = 1.5) +
  geom_errorbarh(aes(xmin = grand_mean_max - grand_se_max, xmax = grand_mean_max + grand_se_max)) +
  geom_point(aes(x = grand_mean_max), shape = 18, size = 2) +
  labs(x = "Observed Suitability", y = "") +
  lims(x = c(0,1)) +
  ggtitle(label = "Establishment Potential (max)") +
  scale_colour_manual(name = "", values = c("jung" = "#C77CFF", "slftoh" = "#00BFC4", "toh" = "#F8766D", "slf" = "#7CAE00"), breaks = c("jung", "slftoh", "toh", "slf"))


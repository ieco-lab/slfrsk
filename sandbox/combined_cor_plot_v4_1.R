#Authors: NAH

#load relevent libraries
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(grid)
library(lattice)
library(here)

rm(list=ls())

# Set these variables to run different versions of the figures.

# Make transport scaled between one and zero (but note that establishment must be uncommented below)
one_zero <- TRUE

# Replace this with max mean quantile_0.50 quantile_0.75 quantile_0.90 to edit which metric to plot for establishment
estab_to_plot <- "max" # max was used for the quadrant figures

# Choose quadrant intercepts
q_intercepts <- list(transport = 0.5, establishment = 0.5) 

# Need a flag to switch if we want present or future infected states transport
present_transport <- FALSE

# Set what type of multiple correlation you want to calculate
method_cor <- "spearman"

# set whether to look at grape yield or wine production primarily
#impact_prod <- "wine"
impact_prod <- "grapes"

# Set the version number here for the pdfs that are being produced
vers <- paste0("4_4_prod_", impact_prod)


# Plot the native countries?
native <- TRUE

# If you want to label both of the countries with established SLF
label.JPN.KOR <- TRUE

# If you want the legend plotted on the states scatter?
# plot_state_legend <- TRUE

#set working dir
#setwd("/Users/nicholashuron/Google Drive File Stream/My Drive/spotted_lanternfly_ieco_projects/")
setwd(here()) #MRH WD

##########################################################################################################################################
#INTERSTATE
##########################################################################################################################################

#MODELS
#read in enm extracts for all four models
states_slf_extract <- read_csv(file = "./data/slf_enm_extract/extract_states_02_15_19_maxent_slf+atc-bio02.csv", col_names = T)
states_toh_extract <- read_csv(file = "./data/slf_enm_extract/extract_states_10_29_18_maxent_toh+atc-bio02.csv", col_names = T)
states_slftoh_extract <- read_csv(file = "./data/slf_enm_extract/extract_states_11_07_18_maxent_slf+toh+atc-bio02.csv", col_names = T)

#clean up the extract data
colnames(states_slf_extract) <- gsub(estab_to_plot, replacement = "obs_max", x = colnames(states_slf_extract))
colnames(states_slf_extract) <- gsub("min", replacement = "obs_min", x = colnames(states_slf_extract))
colnames(states_slf_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(states_slf_extract))
colnames(states_slf_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(states_slf_extract))

#clean up the extract data
colnames(states_toh_extract) <- gsub(estab_to_plot, replacement = "obs_max", x = colnames(states_toh_extract))
colnames(states_toh_extract) <- gsub("min", replacement = "obs_min", x = colnames(states_toh_extract))
colnames(states_toh_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(states_toh_extract))
colnames(states_toh_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(states_toh_extract))

#clean up the extract data
colnames(states_slftoh_extract) <- gsub(estab_to_plot, replacement = "obs_max", x = colnames(states_slftoh_extract))
colnames(states_slftoh_extract) <- gsub("min", replacement = "obs_min", x = colnames(states_slftoh_extract))
colnames(states_slftoh_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(states_slftoh_extract))
colnames(states_slftoh_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(states_slftoh_extract))

#instead, can add model as a categorical var
states_slf_extract <- states_slf_extract %>%
  mutate(model = "slf")
states_toh_extract <- states_toh_extract %>%
  mutate(model = "toh")
states_slftoh_extract <- states_slftoh_extract %>%
  mutate(model = "slftoh")

states_extracts <- bind_rows(states_slf_extract, states_toh_extract, states_slftoh_extract) %>%
  group_by(geopol_unit) %>%
  arrange(geopol_unit)
states_summary_extracts <- states_extracts %>%
  group_by(geopol_unit) %>%
  dplyr::summarize(grand_mean_max = mean(obs_max), grand_se_max = sd(obs_max) / sqrt(n_distinct(model)))


# STOP STOP STOP DECIDE ON THE CSV FILE TO READ IN

if(present_transport==TRUE){  
  
  #TRADE
  states_trade_value <- read_csv(file = "./data/trade/FINAL_DATA/summary_states_trade_value_v2.csv", col_names = T)
  states_trade_mass <- read_csv(file = "./data/trade/FINAL_DATA/summary_states_trade_mass_v2.csv", col_names = T)
  states_trade <- left_join(states_trade_value, states_trade_mass, by = c("destination", "status"), suffix = c("_trade", "_mass"))
  
} else {
  
  #TRADE  
  states_trade_value <- read_csv(file = "./data/trade/FINAL_DATA/summary_states_trade_value_future_v2.csv", col_names = T)
  states_trade_mass <- read_csv(file = "./data/trade/FINAL_DATA/summary_states_trade_mass_future_v2.csv", col_names = T)
  states_trade <- left_join(states_trade_value, states_trade_mass, by = c("destination", "status"), suffix = c("_trade", "_mass"))
  
}

#GRAPES
#yield
#(convert the yield from kg/ha)
states_grapes <- read_csv(file = "./data/grapes/states_grape_yield_2012-2017.csv") %>%
  dplyr::select(State, Year, Value, `Data Item`) %>%
  mutate(State = tolower(State)) %>%
  mutate(State = tools::toTitleCase(State)) %>%
  mutate(Value = Value*2.2417, new_units = "tons/ha")  #convert tons/acre to metric tons/hectare

#summarize the yield
states_summary_grapes <- states_grapes %>%
  group_by(State) %>%
  dplyr::summarize(avg_yield = mean(Value, na.rm = T), se_yield = sd(Value) / sqrt(n_distinct(Year))) %>%
  drop_na(.) %>%
  mutate(log10_avg_yield = log10(avg_yield))

#rename the states col in grape summary to be geopol_unit
colnames(states_summary_grapes)[1] <- "geopol_unit"

#production these data run 1998 - 2019
states_grapes_prod <- read_csv(file = "./data/grapes/NASS_QUICKSTATS_data__states_grapes_production_01-24-2020.csv") %>%
  dplyr::select(State, Year, Value, `Data Item`) %>%
  mutate(State = tolower(State)) %>%
  mutate(State = tools::toTitleCase(State)) %>%
  #filter out to be 2012-2017
  filter(Year > 2011 & Year < 2018)
#change the value to be numeric
states_grapes_prod$Value <- as.numeric(gsub(",","",states_grapes_prod$Value))
#convert from us tons to metric tonnes
states_grapes_prod <- states_grapes_prod %>%
  mutate(Value = Value*1.01605, new_units = "tonnes")

#summarize the production
states_summary_grapes_prod <- states_grapes_prod %>%
  group_by(State) %>%
  dplyr::summarise(avg_prod = mean(Value, na.rm = T), se_prod = sd(Value) / sqrt(n_distinct(Year))) %>%
  drop_na(.) %>%
  mutate(log10_avg_prod = log10(avg_prod))

#rename the states col in grape summary to be geopol_unit
colnames(states_summary_grapes_prod)[1] <- "geopol_unit"

#now combine the two summary objects into one for grapes
states_summary_grapes <- states_summary_grapes_prod %>%
  left_join(., states_summary_grapes, by = "geopol_unit")


#add a binary column for if a state grows grapes or not
states_summary_extracts <- states_summary_extracts %>%
  mutate(grapes = ifelse(geopol_unit %in% states_summary_grapes$geopol_unit, yes = "yes", no = "no")) %>%
  mutate(grapes = factor(grapes, levels = c("yes", "no")))


#wine

#read in data
states_wine <- read_csv("./data/grapes/TTB_data_states_wine.csv")

#clean up the data a little
states_wine <- states_wine %>%
  mutate(geopol_unit = tolower(geopol_unit)) %>%
  mutate(geopol_unit = tools::toTitleCase(geopol_unit))

#convert from gallons to mass in metric tons: conversion on https://www.aqua-calc.com/calculate/food-volume-to-weight#anchor-about
states_wine[,-1] <- states_wine[,-1] * 3.776e-3

#tibble it up
states_wine <- states_wine %>%
  group_by(geopol_unit) %>%
  gather(`2012`:`2017`, key = "Year", value = "Mass")

#need to gather up the wine by year
states_summary_wine <- states_wine %>%
  group_by(geopol_unit) %>%
  dplyr::summarize(avg_wine = mean(Mass, na.rm = T), log10_avg_wine = log10(mean(Mass, na.rm = T)))

#add a binary column for if a state produces wine or not
states_summary_extracts <- states_summary_extracts %>%
  mutate(wine = ifelse(geopol_unit %in% states_summary_wine$geopol_unit, yes = "yes", no = "no")) %>%
  mutate(wine = factor(wine, levels = c("yes", "no")))


####now to find a way to bind trade, extracts, wine

#start with extracts (has grapes yes/no)
states_summary <- states_summary_extracts %>%
  #trade
  left_join(., states_trade, by = c("geopol_unit" = "destination")) %>%
  #wine
  left_join(., states_summary_wine, by = "geopol_unit") %>%
  #grapes
  left_join(., states_summary_grapes, by = "geopol_unit")

#deal with NA
states_summary[is.na(states_summary)] <- 0

#get log10 for everything
states_summary <- states_summary %>%
  mutate(avg_infected_trade = avg_infected_trade + 1, avg_infected_mass = avg_infected_mass + 1, avg_wine = avg_wine + 1) %>%
  mutate(log10_avg_infected_trade = log10(avg_infected_trade), log10_avg_infected_mass = log10(avg_infected_mass), log10_avg_wine = log10(avg_wine))

#add state ID's
stateid <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
stateid <- as_tibble(stateid, ncol =1)
colnames(stateid) <- "ID"

#join the new ID's
states_summary <- bind_cols(stateid, states_summary)

# scale the import data

if(one_zero){
  states_summary <- states_summary %>% 
    # transport
    mutate(avg_infected_mass_scaled = log(avg_infected_mass+1)) %>% 
    mutate(avg_infected_mass_scaled = (avg_infected_mass_scaled-min(avg_infected_mass_scaled))) %>% 
    mutate(avg_infected_mass_scaled = avg_infected_mass_scaled/max(avg_infected_mass_scaled)) %>% 
    # establishment
    mutate(grand_mean_max_scaled = grand_mean_max)
  # mutate(grand_mean_max_scaled = (grand_mean_max-min(grand_mean_max))) %>% 
  # mutate(grand_mean_max_scaled = grand_mean_max_scaled/max(grand_mean_max_scaled))
  
  
} else {
  states_summary <- states_summary %>% 
    # transport
    mutate(avg_infected_mass_scaled = log(avg_infected_mass+1)) # %>% 
  mutate(avg_infected_mass_scaled = scale(avg_infected_mass_scaled)) %>%
    # establishment
    mutate(grand_mean_max_scaled = scale(grand_mean_max))
}
##########################################################################################################################################
#COUNTRIES
##########################################################################################################################################

if(present_transport==TRUE){  
  
  #trade
  countries_trade_value <- read_csv(file = "./data/trade/FINAL_DATA/summary_countries_trade_value.csv", col_names = T)
  countries_trade_mass <- read_csv(file = "./data/trade/FINAL_DATA/summary_countries_trade_mass.csv", col_names = T)
  countries_trade <- left_join(countries_trade_value, countries_trade_mass, by = c("destination"), suffix = c("_trade", "_mass"))
  
} else{
  #trade
  countries_trade_value <- read_csv(file = "./data/trade/FINAL_DATA/summary_countries_trade_value_future.csv", col_names = T)
  countries_trade_mass <- read_csv(file = "./data/trade/FINAL_DATA/summary_countries_trade_mass_future.csv", col_names = T)
  countries_trade <- left_join(countries_trade_value, countries_trade_mass, by = c("destination"), suffix = c("_trade", "_mass"))
  
}

#filter out W. Sahara, which has all zeros
countries_trade <- countries_trade %>%
  filter(destination != "Western Sahara")

#read in enm extracts for all four models
countries_slf_extract <- read_csv(file = "./data/slf_enm_extract/extract_world_11_07_18_maxent_slf+atc-bio02.csv", col_names = T)
countries_toh_extract <- read_csv(file = "./data/slf_enm_extract/extract_world_11_07_18_maxent_toh+atc-bio02.csv", col_names = T)
countries_slftoh_extract <- read_csv(file = "./data/slf_enm_extract/extract_world_11_07_18_maxent_slf+toh+atc-bio02.csv", col_names = T)

#instead, can add model as a categorical var
countries_slf_extract <- countries_slf_extract %>%
  mutate(model = "slf")
countries_toh_extract <- countries_toh_extract %>%
  mutate(model = "toh")
countries_slftoh_extract <- countries_slftoh_extract %>%
  mutate(model = "slftoh")

#all rows with NA appear to also have the other issues, so we can just use drop_na()
countries_slf_extract <- drop_na(countries_slf_extract)
countries_toh_extract <- drop_na(countries_toh_extract)
countries_slftoh_extract <- drop_na(countries_slftoh_extract)

#clean up the extract data
colnames(countries_slf_extract) <- gsub(estab_to_plot, replacement = "obs_max", x = colnames(countries_slf_extract))
colnames(countries_slf_extract) <- gsub("min", replacement = "obs_min", x = colnames(countries_slf_extract))
colnames(countries_slf_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(countries_slf_extract))
colnames(countries_slf_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(countries_slf_extract))
#clean up the extract data
colnames(countries_toh_extract) <- gsub(estab_to_plot, replacement = "obs_max", x = colnames(countries_toh_extract))
colnames(countries_toh_extract) <- gsub("min", replacement = "obs_min", x = colnames(countries_toh_extract))
colnames(countries_toh_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(countries_toh_extract))
colnames(countries_toh_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(countries_toh_extract))
#clean up the extract data
colnames(countries_slftoh_extract) <- gsub(estab_to_plot, replacement = "obs_max", x = colnames(countries_slftoh_extract))
colnames(countries_slftoh_extract) <- gsub("min", replacement = "obs_min", x = colnames(countries_slftoh_extract))
colnames(countries_slftoh_extract) <- gsub("mean", replacement = "obs_mean", x = colnames(countries_slftoh_extract))
colnames(countries_slftoh_extract) <- gsub("sd", replacement = "obs_sd", x = colnames(countries_slftoh_extract))

countries_extracts <- bind_rows(countries_slf_extract, countries_toh_extract, countries_slftoh_extract) %>%
  group_by(geopol_unit) %>%
  arrange(geopol_unit)

#filter out W. Sahara, which has all zeros for other categories
countries_extracts <- countries_extracts %>%
  filter(geopol_unit != "Western Sahara")


#get MEAN AND SE across models
countries_summary_extracts <- countries_extracts %>%
  group_by(geopol_unit) %>%
  dplyr::summarize(grand_mean_max = mean(obs_max, na.rm = T), grand_se_max = sd(obs_max) / sqrt(n_distinct(model)))

#grapes
#YIELD
countries_grapes <- read_csv(file = "./data/grapes/FAOSTAT_data_4-22-2019.csv") %>%
  dplyr::select(Area, Item, Year, Value, Unit) %>%
  mutate(Value = Value*0.0001, new_units = "tons/ha") %>%   #convert hg/hectare to metric tons/hectare
  filter(Area != "Western Sahara")  

#rename Area to geopol_unit
colnames(countries_grapes)[1] <- "geopol_unit"

#change the name of United States of America to United States
countries_grapes$geopol_unit <- gsub(pattern = "United States of America", replacement = "United States", x = countries_grapes$geopol_unit)

countries_summary_grapes <- countries_grapes %>%
  group_by(geopol_unit) %>%
  dplyr::summarize(avg_yield = mean(Value, na.rm = T), se_yield = sd(Value) / sqrt(n_distinct(Year))) %>%
  drop_na(.) %>%
  mutate(log10_avg_yield = log10(avg_yield))

#production
countries_grapes_prod <- read_csv("./data/grapes/FAOSTAT_data_countries_grapes_production_01-24-2020.csv")

colnames(countries_grapes_prod)[colnames(countries_grapes_prod) %in% "Area"] <- "geopol_unit"

countries_grapes_prod <- countries_grapes_prod %>%
  dplyr::select(geopol_unit, Year, Value, Unit, Item) %>%
  #filter out to be 2012-2017
  filter(Year > 2011 & Year < 2018)

#summarize the production
countries_summary_grapes_prod <- countries_grapes_prod %>%
  group_by(geopol_unit) %>%
  dplyr::summarise(avg_prod = mean(Value, na.rm = T), se_prod = sd(Value) / sqrt(n_distinct(Year))) %>%
  drop_na(.) %>%
  mutate(log10_avg_prod = log10(avg_prod))

#now combine the two summary objects into one for grapes
countries_summary_grapes <- countries_summary_grapes_prod %>%
  left_join(., countries_summary_grapes, by = "geopol_unit")

#add a binary column for if a country grows grapes or not
countries_summary_extracts <- countries_summary_extracts %>%
  mutate(grapes = ifelse(geopol_unit %in% countries_summary_grapes$geopol_unit, yes = "yes", no = "no")) %>%
  mutate(grapes = factor(grapes, levels = c("yes", "no")))

#change U.S. from no to yes
countries_summary_extracts$grapes[countries_summary_extracts$geopol_unit == "United States"] <- "yes"



#wine
countries_wine <- read_csv("./data/grapes/FAOSTAT_data_wine_6-21-2019_v2.csv")

#clean up the countries data
countries_wine <- countries_wine %>%
  mutate(geopol_unit = Area, Mass = Value) %>%
  dplyr::select(geopol_unit, Year, Mass, Unit)

#need to gather up the wine by year and make sure to remove -inf/NaN generated by countries that do not produce any wine
countries_summary_wine <- countries_wine %>%
  group_by(geopol_unit) %>%
  dplyr::summarize(avg_wine = mean(Mass, na.rm = T), log10_avg_wine = log10(mean(Mass, na.rm = T))) %>%
  filter(!is.infinite(log10_avg_wine) & !is.nan(log10_avg_wine))

#add a binary column for if a country produces wine or not
countries_summary_extracts <- countries_summary_extracts %>%
  mutate(wine = ifelse(geopol_unit %in% countries_summary_wine$geopol_unit, yes = "yes", no = "no")) %>%
  mutate(wine = factor(wine, levels = c("yes", "no")))


####now to find a way to bind trade, extracts, wine

#start with extracts (has grapes yes/no)
countries_summary <- countries_summary_extracts %>%
  #trade
  left_join(., countries_trade, by = c("geopol_unit" = "destination")) %>%
  #wine
  left_join(., countries_summary_wine, by = "geopol_unit") %>%
  #grapes
  left_join(., countries_summary_grapes, by = "geopol_unit")

#deal with NA
countries_summary[is.na(countries_summary)] <- 0

#get log10 for everything
countries_summary <- countries_summary %>%
  mutate(avg_infected_trade = avg_infected_trade + 1, avg_infected_mass = avg_infected_mass + 1, avg_wine = avg_wine + 1) %>%
  mutate(log10_avg_infected_trade = log10(avg_infected_trade), log10_avg_infected_mass = log10(avg_infected_mass), log10_avg_wine = log10(avg_wine))


# add a column for status establishment
countries_summary <- countries_summary %>%  mutate(status = "not established")

if(native){
  countries_summary$status[countries_summary$geopol_unit %in% c("China", "India", "Taiwan", "Vietnam")] <- "native"
} else {countries_summary$status[countries_summary$geopol_unit %in% c("China", "India", "Taiwan", "Vietnam")] <- "not established"
}

countries_summary$status[countries_summary$geopol_unit %in% c("Japan", "South Korea", "United States")] <- "established"

# add ID to make is simpler for labeling points
countries_summary <- countries_summary %>%  mutate(ID = geopol_unit)

# remove junk "countries" NOTE USA IS REMOVED HERE
#Philippines is also removed, as the grape yield data for it was several orders of magnitude smaller than anything else reported and so close to zero that it caused log transformation scaling issues
if(TRUE){ # TRUE if you want to remove the USA
  rems <- c("Akrotiri and Dhekelia"
            ,"?land"
            ,"American Samoa"
            ,"Bouvet Island"
            ,"British Indian Ocean Territory"
            ,"Caspian Sea"
            ,"Christmas Island"
            ,"Clipperton Island"
            ,"Cocos Islands"
            ,"Falkland Islands"
            ,"Faroe Islands"
            ,"French Southern Territories"
            ,"Heard Island and McDonald Islands"
            ,"Mayotte"
            ,"Northern Mariana Islands"
            ,"Paracel Islands"
            ,"Pitcairn Islands"
            ,"Saint Pierre and Miquelon"
            ,"Tokelau"
            ,"United States Minor Outlying Islands"
            ,"Wallis and Futuna"
            ,"United States"
            ,"Philippines")
} else {
  rems <- c("Akrotiri and Dhekelia"
            ,"?land"
            ,"American Samoa"
            ,"Bouvet Island"
            ,"British Indian Ocean Territory"
            ,"Caspian Sea"
            ,"Christmas Island"
            ,"Clipperton Island"
            ,"Cocos Islands"
            ,"Falkland Islands"
            ,"Faroe Islands"
            ,"French Southern Territories"
            ,"Heard Island and McDonald Islands"
            ,"Mayotte"
            ,"Northern Mariana Islands"
            ,"Paracel Islands"
            ,"Pitcairn Islands"
            ,"Saint Pierre and Miquelon"
            ,"Tokelau"
            ,"United States Minor Outlying Islands"
            ,"Wallis and Futuna")
  
}
length(rems)
dim(countries_summary)
countries_summary <- countries_summary %>% filter(!ID %in% rems)

# scale the import data

if(one_zero){
  countries_summary <- countries_summary %>% 
    # transport
    mutate(avg_infected_mass_scaled = log10_avg_infected_mass) %>% 
    mutate(avg_infected_mass_scaled = (avg_infected_mass_scaled-min(avg_infected_mass_scaled))) %>% 
    mutate(avg_infected_mass_scaled = avg_infected_mass_scaled/max(avg_infected_mass_scaled)) %>%
    # establishment
    mutate(grand_mean_max_scaled = grand_mean_max)
  #    mutate(grand_mean_max_scaled = (grand_mean_max-min(grand_mean_max))) %>% 
  #    mutate(grand_mean_max_scaled = grand_mean_max_scaled/max(grand_mean_max_scaled))
  
} else{
  countries_summary <- countries_summary %>% 
    # transport
    mutate(avg_infected_mass_scaled = log10_avg_infected_mass) %>% 
    mutate(avg_infected_mass_scaled = scale(avg_infected_mass_scaled)) # %>%
  
  # establishment
  mutate(grand_mean_max_scaled = scale(grand_mean_max))
  
}


# COUNTRY PLOTS -------------------------------------------------------------

# #plot all of it  
# ggplot(data = countries_summary) +
#   #add points for enm x trade
#   #geom_point(aes(x = avg_infected_mass, y = grand_mean_max, color = grapes, size = log10_avg_wine)) +
#   geom_point(aes(x = avg_infected_mass, y = grand_mean_max, color = grapes, size = avg_wine)) +
#   stat_smooth(aes(x = avg_infected_mass, y = grand_mean_max), method = "lm", fullrange = TRUE) +
#   scale_color_manual(values = c("no" = "#808080", "yes" = "#C77CFF"), name = "", labels = c("grapes", "no grapes")) +
#   scale_size_continuous(name = "Wine Production", trans = "log10") +
#   labs(x = "Imports (Metric Tons)", y = "Observed Max Suitability") +
#   scale_x_log10() +
#   geom_text_repel(aes(x = avg_infected_mass, 
#                       y = grand_mean_max,
#                       label = ID), 
#                  min.segment.length = 0)
#   


# #plot all of it
# countries_summary %>%
#   filter(log10_avg_infected_mass != 0) %>%
#   ggplot(data = .) +
#   #add points for enm x trade
#   geom_point(aes(x = avg_infected_mass, y = grand_mean_max, color = grapes, size = avg_wine)) +
#   stat_smooth(aes(x = avg_infected_mass, y = grand_mean_max), method = "lm", fullrange = TRUE) +
#   scale_color_manual(values = c("no" = "#808080", "yes" = "#C77CFF"), name = "", labels = c("grapes", "no grapes")) +
#   scale_size_continuous(name = "Wine Production", trans = "log10") +
#   labs(x = "Imports (Metric Tons)", y = "Observed Max Suitability") +
#   scale_x_log10()

countries_summary_nozeros <- countries_summary %>% filter(log10_avg_infected_mass != 0)

countries_summary_nozeros <- countries_summary_nozeros %>% mutate(avg_infected_mass_scaled = log(avg_infected_mass+1)) %>% mutate(avg_infected_mass_scaled = (avg_infected_mass_scaled-min(avg_infected_mass_scaled))) %>% mutate(avg_infected_mass_scaled = avg_infected_mass_scaled/max(avg_infected_mass_scaled))


##############################################################
#### PLOTS --------------------------------------
###############################################################
##############################################################
#### PLOTS --------------------------------------
###############################################################
##############################################################
#### PLOTS --------------------------------------
###############################################################
##############################################################
#### PLOTS --------------------------------------
###############################################################
##############################################################
#### PLOTS --------------------------------------
###############################################################

hh = .01 #set margins
plots_which <- c(TRUE,FALSE) # Do not change this flips beteen potting the states or plotting countries
top_to_plot <- 10 # How many countries and states to highlight for labels?
nudge_JPNKOR.y <- c(.1,.1)
nudge_JPNKOR.x <- c(-.17,0)
lx <- 0.22
ly <- 0.25
hy <- 0.77
hx <- 0.78
risk_color <- "gray40"
risk_size <- 5

risk_labels <-data.frame(label = c("low\nrisk","moderate\nrisk","moderate\nrisk","high\nrisk"),
                         x = c(lx,lx,hx,hx),
                         y = c(ly,hy,ly,hy),
                         hjust = c(0,0,0,0),
                         vjust = c(0,0,0,0))

i<-1
for (i in 1:length(plots_which)) {
  # for (i in 1) {
  #  for (i in 2) {
  statez = plots_which[i]
  
  if(impact_prod == "wine"){
    if (statez) {
      data_to_plot <- states_summary
      data_to_plot <- data_to_plot %>%
        mutate(
          x_to_plot = avg_infected_mass_scaled,
          y_to_plot = grand_mean_max_scaled,
          fill_to_plot = grapes,
          size_to_plot = avg_wine,
          color_to_plot = status
        ) %>%
        arrange(desc(grapes), (size_to_plot)) #so that the grape producers are on top
      data_to_label <-
        tail(data_to_plot, top_to_plot)  %>% arrange(desc(size_to_plot))
      state_grape_t.test <- list(transport = t.test(avg_infected_mass_scaled~grapes, data = data_to_plot),
                                 establishment = t.test(grand_mean_max_scaled~grapes, data = data_to_plot))
    } else {
      data_to_plot <- countries_summary
      data_to_plot <- data_to_plot %>%
        mutate(
          x_to_plot = avg_infected_mass_scaled,
          y_to_plot = grand_mean_max_scaled,
          fill_to_plot = grapes,
          size_to_plot = avg_wine,
          color_to_plot = status
        ) %>%
        arrange(desc(grapes), (size_to_plot)) #so that the grape producers are on top
      data_to_label <-
        tail(data_to_plot, top_to_plot)  %>% 
        arrange(desc(size_to_plot)) %>%
        mutate(
          ID = recode(
            ID,
            Italy = "ITA",
            France = "FRA",
            Spain = "ESP",
            China = "CHN",
            Argentina = "ARG",
            Chile = "CHL",
            Australia = "AUS",
            `South Africa` = "ZAF",
            Germany = "DEU",
            Portugal = "PRT"
          )
        )
      
      country_grape_t.test <- list(transport = t.test(avg_infected_mass_scaled~grapes, data = data_to_plot),
                                   establishment = t.test(grand_mean_max_scaled~grapes, data = data_to_plot))
      if(label.JPN.KOR){
        data_to_label_JPNKOR <- data_to_plot %>%
          filter(geopol_unit %in% c("Japan", "South Korea")) %>% 
          mutate(ID = recode(ID,
                             Japan = "JPN",
                             `South Korea` = "KOR")) %>%
          arrange(desc(size_to_plot))
        data_to_label <- bind_rows(data_to_label,data_to_label_JPNKOR)
      }
    }
  } else if(impact_prod == "grapes"){
    if (statez) {
      data_to_plot <- states_summary
      data_to_plot <- data_to_plot %>%
        mutate(
          x_to_plot = avg_infected_mass_scaled,
          y_to_plot = grand_mean_max_scaled,
          fill_to_plot = wine,
          size_to_plot = avg_prod, #avg_yield or avg_prod are the two options here for grapes
          color_to_plot = status
        ) %>%
        arrange(desc(grapes), (size_to_plot)) #so that the grape producers are on top
      #change the zeros to one's for plot size
      data_to_plot$size_to_plot[data_to_plot$size_to_plot == 0] <- 1
      
      data_to_label <-
        tail(data_to_plot, top_to_plot)  %>% arrange(desc(size_to_plot))
      state_grape_t.test <- list(transport = t.test(avg_infected_mass_scaled~grapes, data = data_to_plot),
                                 establishment = t.test(grand_mean_max_scaled~grapes, data = data_to_plot))
    } else {
      data_to_plot <- countries_summary
      data_to_plot <- data_to_plot %>%
        mutate(
          x_to_plot = avg_infected_mass_scaled,
          y_to_plot = grand_mean_max_scaled,
          fill_to_plot = wine, 
          size_to_plot = avg_prod, #avg_yield or avg_prod are the two options here for grapes
          color_to_plot = status
        ) %>%
        arrange(desc(grapes), (size_to_plot)) #so that the grape producers are on top
      #change the zeros to one's for plot size
      data_to_plot$size_to_plot[data_to_plot$size_to_plot == 0] <- 1
      
      data_to_label <-
        tail(data_to_plot, top_to_plot)  %>% arrange(desc(size_to_plot))
      data_to_label <- data_to_label %>%
        mutate(
          ID = recode(
            ID,
            Egypt = "EGY",
            Peru = "PER",
            India = "IND",
            Albania = "ALB",
            Iraq = "IRQ",
            Brazil = "BRA",
            Thailand = "THA",
            `South Africa` = "ZAF",
            China = "CHN",
            Armenia = "ARM",
            Italy = "ITA",
            Spain = "ESP",
            France = "FRA",
            Turkey = "TUR",
            Chile = "CHL",
            Argentina = "ARG",
            Australia = "AUS",
            `South Korea` = "KOR",
            Japan = "JPN"
          )
        )
      country_grape_t.test <- list(transport = t.test(avg_infected_mass_scaled~grapes, data = data_to_plot),
                                   establishment = t.test(grand_mean_max_scaled~grapes, data = data_to_plot))
      if(label.JPN.KOR){
        data_to_label_JPNKOR <- data_to_plot %>%
          filter(geopol_unit %in% c("Japan", "South Korea")) %>% 
          mutate(ID = recode(ID,
                             Japan = "JPN",
                             `South Korea` = "KOR")) %>%
          arrange(desc(size_to_plot))
        data_to_label <- bind_rows(data_to_label,data_to_label_JPNKOR)
      } # end of if label plotting JPN KOR
    }
  }
  
  
  
  #plot all of it
  #need to add the conditional for present transport, since nudging only works based on the data
  if(present_transport == FALSE){
    if(impact_prod == "wine"){
      #nudging
      if(statez) {
        nudge.y <- c(.2,.10,.10,.10,.07,.15,.15,-.07,-.12,-.12)    #CA, WA, NY, PA, OR, GA, OH, MI, VA, NC
        nudge.x <- c(-.1,-.1,-.1,-.05,-.1,-0.05,-.1,.06,-.01,.05)
        ytitle <- "Establishment Potential"
      } else {
        nudge.y <- c(.13, .095, .155, .09, -.11, .13, .15,.06,-.1,-.19)  #ITA, FRA, ESP, CHN, ARG, CHL, AUS, ZAF, DEU, PRT 
        nudge.x <- c(-.3,-.27,-.05,-.26,.14,.1,.20,-.21,.08,.2)
        if(label.JPN.KOR) {
          nudge.x <- c(nudge.x, nudge_JPNKOR.x) #JPN KOR
          nudge.y <- c(nudge.y, nudge_JPNKOR.y) #JPN KOR
        }
        ytitle <- "Establishment Potential"
      }
    } else if(impact_prod == "grapes"){
      #nudging
      if(statez) {
        nudge.y <- c( .20, .10, .10, .10,-.05, .10,-.05,-.07,-.12, .10)   #production: CA, WA, NY, PA, MI, OR, TX, VA, NC, MO
        nudge.x <- c(-.10,-.05,-.22,-.05, .10,-.05, .10,.025, .05,-.025)   #yield: #CA, PA, WA, MI, NY, MO, OH, OR, GA, NC
        ytitle <- "Establishment Potential"
      } else {
        # grape production: CHN, ITA, ESP, FRA, TUR, IND, CHL, ARG, ZAF,   AUS
        nudge.y <-       c( .10, .11, .12, .09,-.05, .10,.125,-.06,-.1,-.07)   
        nudge.x <-       c(-.05,-.25,-.02,-.06, .01,-.05,.200, .10, .075, .145)
        if(label.JPN.KOR) {
          nudge.x <- c(nudge.x, nudge_JPNKOR.x) #JPN KOR
          nudge.y <- c(nudge.y, nudge_JPNKOR.y) #JPN KOR
        }
        ytitle <- "Establishment Potential"
      } 
    }
    
  } else { # grapes above wine below NOTE will not work with labeling JPN and KOR
    if(statez) {
      nudge.y <- c(-.10,.10,.10,.10,.07,.10,.12,-.07,-.12,-.12)    #CA, WA, NY, PA, OR, GA, OH, MI, VA, NC
      nudge.x <- c(.09,-.1,-.04,-.05,-.10,-0.12,-.1,.04,-.01,.05)
      ytitle <- "Establishment Potential"
    } else {
      nudge.y <- c(.13, .09, .155, .09, -.11, .13, .15,.09,-.08,-.15)  #ITA, FRA, ESP, CHN, ARG, CHL, AUS, ZAF, DEU, PRT
      nudge.x <- c(-.03,-.10,-.02,-.03,.14,-.1,-.23,-.21,.08,.2)
      ytitle <- "Establishment Potential"
    }
  }
  
  (states_plot <- ggplot(data = data_to_plot) +
      geom_text(data = risk_labels,
               mapping = aes(x = x, y = y,label = label),
               color = risk_color,
               size = risk_size) +
      #geom_segment(aes(x = 0, xend = 1, y = .5, yend = .5), linetype = "dashed") +
      #geom_segment(aes(y = 0, yend = 1, x = .5, xend = .5), linetype = "dashed") +
      geom_hline(yintercept = q_intercepts$establishment, linetype = "dashed") +
      geom_vline(xintercept = q_intercepts$transport, linetype = "dashed") +
      #  geom_abline(intercept = 0, slope = 1, linetype = "solid") + # add one to one
      geom_rect(mapping = aes(xmin=1.01, xmax=1.2, ymin=.49, ymax=.5), fill = "white") +
      geom_rect(mapping = aes(ymin=1.01, ymax=1.2, xmin=.49, xmax=.51), fill = "white") +
      geom_text_repel(data = data_to_label,
                      aes(x = x_to_plot, y = y_to_plot, label = ID),
                      min.segment.length = 0,
                      direction = "x",
                      nudge_y = nudge.y,
                      nudge_x = nudge.x
      ) +
      geom_point(
        aes(x = x_to_plot, y = y_to_plot,
            fill = fill_to_plot, size = size_to_plot, color = color_to_plot),
        shape = 21, stroke = 1.3, alpha = 0.75
      ) +
      # geom_text(data = risk_labels, 
      #           mapping = aes(x = x, y = y,label = label), 
      #           color = risk_color,
      #           size = risk_size) +
      scale_fill_manual(
        values = c("no" = "#ffffff", "yes" = "#C77CFF"),
        name = "Wine Production",# "Wine Impact Potential",
        labels = c("high", "low")
      ) +
      scale_color_manual(
        values = c(
          "established" = "red",
          "not established" = "black",
          "native" = "blue"),
        name = "Regional Status",
        labels = c("invaded","native", "uninvaded")
      ) +
      guides(
        fill = guide_legend(
          order = 1,
          override.aes = list(
            shape = 22,
            size = 5,
            alpha = 1
          )
        ), 
        color = guide_legend(
          order = 3
        ),
        size = guide_legend(    # Adjust size to edit legend grape production 
          order = 2,            # circles.
          override.aes = list(
            size = c(1,3,6)
          )
        )
      ) +
      scale_size_continuous(name = "Grape Production", 
                            trans = "log10", 
                            range = c(1, 6),
                            breaks = c(1,2,6),
                            labels = c("low", "moderate", "high")
      ) +
      labs(x = "Transport Potential", y = ytitle) +
      ylim(0, 1.2) + xlim(0, 1.2) +
      theme(
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.7, 0.25),
        #legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),#element_rect(colour = 'black', fill = 'white', linetype='solid'),
        axis.text = element_text(size = rel(1)),
        axis.title.x = element_text(hjust = .4),
        axis.title.y = element_text(hjust = .35),
        legend.title = element_text(face = "plain"),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_blank(),
        plot.margin = unit(c(hh, hh, hh, hh), units = "line"),
        axis.title = element_text(size = rel(1.3))
      ) + 
      scale_x_continuous(breaks = c(0,.5,1),labels = c("low", "moderate", "high")) + 
      scale_y_continuous(breaks = c(0,.5,1),labels = c("low", "moderate", "high")) 
      #+ coord_flip()
  )
  
  #get the legend
  get_legend <- function(myggplot) {
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <-
      which(sapply(tmp$grobs, function(x)
        x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  #plot all of it
  if(statez) {
    legend_state <- legend <- get_legend(states_plot)
  } else {
    legend_country <- legend <- get_legend(states_plot)
  }
  
  
  (states_plot <- states_plot + theme(legend.position = "none"))
  
  
  #boxplot
  (
    states_box_estab <- ggplot(data_to_plot) +
      geom_boxplot(
        aes(
          x = fill_to_plot,
          y = y_to_plot,
          group = fill_to_plot,
          fill = fill_to_plot
        ),
        show.legend = FALSE,
        #outlier.size = 0,
        notch = FALSE,
        notchwidth = .25
      ) +
      #geom_dotplot(aes(x = fill_to_plot, y = y_to_plot, group = fill_to_plot, fill = fill_to_plot),
      #             binaxis='y',
      #             stackdir='center',
      #             dotsize=1) +
      # geom_point(aes(x = fill_to_plot,
      #                y = y_to_plot,
      #                fill = fill_to_plot,
      #                size = log10(size_to_plot),
      #                color = color_to_plot),
      #            shape = 21,
      #            stroke = 1.3) +
    #labs(x = "Major grape producer", y = "Observed Max Suitability") +
    ylim(0, 1) +
      scale_fill_manual(
        values = c("yes" = "#C77CFF", "no" = "#ffffff"),
        name = ""
      ) +
      #ggtitle(label = "States Grape Production vs. Average Model Maxes") +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(hh, hh, hh, hh), units = "line"),
        plot.background = element_blank()
      )
  )
  
  #ggplot(data = data_to_plot) + geom_density(mapping = aes(x_to_plot, fill = fill_to_plot))
  #boxplot
  (
    states_box_intro <- ggplot(data_to_plot) +
      geom_boxplot(
        aes(
          x = fill_to_plot,
          y = x_to_plot,
          group = fill_to_plot,
          fill = fill_to_plot
        ),
        show.legend = FALSE,
        #outlier.size = 0,
        notch = FALSE,
        notchwidth = .25
      ) +
      # geom_point(aes(x = fill_to_plot,
      #                y = x_to_plot,
      #                fill = fill_to_plot,
      #                size = log10(size_to_plot),
      #                color = color_to_plot),
      #            shape = 21,
      #            stroke = 1.3) +
      #geom_density(aes(x_to_plot, fill = fill_to_plot), show.legend = F, alpha = 0.6) +
      labs(x = "  ", y = "Observed Max Suitability") +
      ylim(0, 1) +
      scale_fill_manual(
        values = c("yes" = "#C77CFF", "no" = "#ffffff"),
        name = ""
      ) +
      coord_flip() +
      #ggtitle(label = "States Grape Production vs. Average Model Maxes") +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        #axis.title.y=element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(hh, hh, hh, hh), units = "line")
      )
  )
  
  
  #arrange it in a nice combined grid
  
  #grid.arrange(states_box_intro, grid.text(""), states_plot, states_box_estab, nrow = 2, ncol = 2, widths = c(5, 1), heights = c(1,5))
  
  
  
  # Insert xbp_grob inside the scatter plot
  #x = x_to_plot,
  #y = y_to_plot
  (
    states_plot_final <- states_plot +
      # annotation_custom(grob = legend, xmin = .465, ymax = .6) +
      #coord_cartesian(clip = "off") +
      annotation_custom(
        grob = ggplotGrob(states_box_estab),
        xmin = 1.01,
        xmax = 1.11,
        ymin = -Inf,
        ymax = 1.05
      ) +
      # Insert ybp_grob inside the scatter plot
      annotation_custom(
        grob = ggplotGrob(states_box_intro),
        xmin = -.1,
        xmax = 1.05,
        ymin = 1,
        ymax = 1.1
      ) +
      
      annotation_custom(
        grob = rectGrob(gp = gpar(fill = "white", col = "white")),
        xmin = -1,
        xmax = 1.5,
        ymin = 1.1,
        ymax = 1.5
      ) +
      annotation_custom(
        grob = rectGrob(gp = gpar(fill = "white", col = "white")),
        ymin = -1,
        ymax = 1.5,
        xmin = 1.11,
        xmax = 1.5
      )
  )
  
  g <- ggplotGrob(states_plot_final)
  g$layout$clip[g$layout$name == "panel"] <- "off"
  #g$layout
  g$layout$z[g$layout$name == "panel"] <-
    17 # Note that z for panel is 1.  Change it to something bigger.
  
  grid.newpage()
  grid.draw(g)
  
  # present or future 
  if(present_transport){
    
    if (statez) {
      states_grob <- g
      pdf(paste0("./submission/Global_MAXENT/figures/states_combined_",estab_to_plot,"_v",vers,".pdf"), 
          width = 7.5, height = 7)
      grid.draw(g)
      dev.off()
      data_to_cor <- as.matrix(dplyr::select(data_to_plot,x_to_plot,y_to_plot,size_to_plot))
      states.impact.model <-
        impact.model <-
        lm(size_to_plot ~ x_to_plot + y_to_plot, data = data.frame(data_to_cor))
      states.impact.cor <- cor.test(impact.model$model$size_to_plot,
                                    impact.model$fitted.values,
                                    method = method_cor)
    } else {
      countries_grob <- g
      pdf(paste0("./submission/Global_MAXENT/figures/countries_combined_",estab_to_plot,"_v",vers,".pdf"), 
          width = 7.5, height = 7)
      grid.draw(g)
      dev.off()
      data_to_cor <- as.matrix(dplyr::select(data_to_plot,x_to_plot,y_to_plot,size_to_plot))
      countries.impact.model <-
        impact.model <-
        lm(size_to_plot ~ x_to_plot + y_to_plot, data = data.frame(data_to_cor))
      countries.impact.cor <- cor.test(impact.model$model$size_to_plot,
                                       impact.model$fitted.values,
                                       method = method_cor)
      
      
    }
  } else {
    
    
    if (statez) {
      states_grob <- g
      pdf(paste0("./submission/Global_MAXENT/figures/states_combined_future_",estab_to_plot,"_v",vers,".pdf"), 
          width = 7.5, height = 7)
      grid.draw(g)
      dev.off()
      data_to_cor <- as.matrix(dplyr::select(data_to_plot,x_to_plot,y_to_plot,size_to_plot))
      states.impact.model <-
        impact.model <-
        lm(size_to_plot ~ x_to_plot + y_to_plot, data = data.frame(data_to_cor))
      states.impact.cor <- cor.test(impact.model$model$size_to_plot,
                                    impact.model$fitted.values,
                                    method = method_cor)
    } else {
      countries_grob <- g
      pdf(paste0("./submission/Global_MAXENT/figures/countries_combined_future_",estab_to_plot,"_v",vers,".pdf"), 
          width = 7.5, height = 7)
      grid.draw(g)
      dev.off()
      data_to_cor <- as.matrix(dplyr::select(data_to_plot,x_to_plot,y_to_plot,size_to_plot))
      countries.impact.model <-
        impact.model <-
        lm(size_to_plot ~ x_to_plot + y_to_plot, data = data.frame(data_to_cor))
      countries.impact.cor <- cor.test(impact.model$model$size_to_plot,
                                       impact.model$fitted.values,
                                       method = method_cor)
      
      
      
    }
  }
} # end of the 2 loop for loop for plotting states and countries

pdf(paste0("./submission/Global_MAXENT/figures/legend_combined_v",vers,".pdf"), 
    width = 3, height = 8)
grid.draw(legend)
dev.off()


# if(plot_state_legend)
# {
#   pdf(paste0("./submission/Global_MAXENT/figures/states_combined_future_legend",estab_to_plot,"_v",vers,".pdf"), 
#       width = 7.5, height = 7)
#   #grid.newpage()
#   grid.draw(states_grob)
#   #vp <- viewport(x=.62, y=.32, 
#   #               width=.6, 
#   #               height=.3)
#   #pushViewport(vp)
#   annotation_custom(legend_country)
#   upViewport()
#   dev.off()
#   
# }


# grid.arrange(states_grob, countries_grob, legend, nrow = 1, ncol = 3, widths = c(5, 5, 1), heights = c(5))

states.impact.cor
countries.impact.cor
state_grape_t.test
country_grape_t.test


# END PLOT LOOP


# grid.arrange(states_grob, countries_grob, legend, nrow = 1, ncol = 3, widths = c(5, 5, 1), heights = c(5))

if(FALSE){
  pdf("./submission/Global_MAXENT/figures/state_legend_combined_v2_5.pdf", 
      width = 3, height = 8)
  grid.draw(legend_state)
  dev.off()
  
  pdf("./submission/Global_MAXENT/figures/country_legend_combined_v2_5.pdf", 
      width = 3, height = 8)
  grid.draw(legend_country)
  dev.off()
}




#other multiple correlation tests
if(FALSE){
  
  
  #library("Hmisc")
  #res2 <- rcorr(data_to_cor, type = "spearman")
  #res2
  
  intro.model <- lm(x_to_plot ~ y_to_plot + size_to_plot, 
                    data = data.frame(data_to_cor))
  cor.test(intro.model$model$x_to_plot, 
           intro.model$fitted.values, method = "spearman")
  
  estab.model <- lm(y_to_plot ~ x_to_plot + size_to_plot, 
                    data = data.frame(data_to_cor))
  cor.test(estab.model$model$y_to_plot, 
           estab.model$fitted.values, method = "spearman")
  
  impact.model <- lm(size_to_plot ~ x_to_plot + y_to_plot, 
                     data = data.frame(data_to_cor))
  cor.test(impact.model$model$size_to_plot, 
           impact.model$fitted.values, method = "spearman")
  
  
  #arrange it in a nice combined grid
  
}





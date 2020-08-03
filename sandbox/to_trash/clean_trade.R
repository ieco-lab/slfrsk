#Clean Trade Data

#Authors: NAH

#load relevent libraries
library(tidyverse)

#set working dir
setwd("/Users/nicholashuron/Google Drive File Stream/My Drive/spotted_lanternfly_ieco_projects/data/trade/")

#read in interstate trade
states <- read_csv("./FAFdata_states_13_june_2019.csv")
states_mass <- read_csv("./FAFdata_states_tons_13_june_2019.csv")

#clean up col labels and order, then get some summary data
#note that this step turns value units for states to USD
states <- states %>%
  mutate(origin = `DMS ORIG`, 
         destination = `DMS DEST`, 
         value_2012 = 1e6*(`Total M$ in 2012`), 
         value_2013 = 1e6*(`Total M$ in 2013`), 
         value_2014 = 1e6*(`Total M$ in 2014`), 
         value_2015 = 1e6*(`Total M$ in 2015`), 
         value_2016 = 1e6*(`Total M$ in 2016`), 
         value_2017 = 1e6*(`Total M$ in 2017`)) %>%
  dplyr::select(origin, destination, SCTG2, value_2012, value_2013, value_2014, value_2015, value_2016, value_2017) %>%
  group_by(origin, destination, SCTG2) %>%
  mutate(sum = sum(value_2012, value_2013, value_2014, value_2015, value_2016, value_2017, na.rm = T), average = mean(x = c(value_2012, value_2013, value_2014, value_2015, value_2016, value_2017), na.rm = T))

#note that this step turns the mass units for states_mass to kg (1 kTon = 907184.7 kg)
states_mass <- states_mass %>%
  mutate(origin = `DMS ORIG`,
         destination = `DMS DEST`,
         mass_2012 = 907184.7*(`Total KTons in 2012`),
         mass_2013 = 907184.7*(`Total KTons in 2013`),
         mass_2014 = 907184.7*(`Total KTons in 2014`),
         mass_2015 = 907184.7*(`Total KTons in 2015`),
         mass_2016 = 907184.7*(`Total KTons in 2016`),
         mass_2017 = 907184.7*(`Total KTons in 2017`)) %>%
  dplyr::select(origin, destination, SCTG2,mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017) %>%
  group_by(origin, destination, SCTG2) %>%
  mutate(sum = sum(mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017, na.rm = T), average = mean(x = c(mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017), na.rm = T))

#states
states_total <- states %>%
  dplyr::select(-sum, -average) %>%
  group_by(origin, destination) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  group_by(origin, destination) %>%
  mutate(sum = sum(value_2012, value_2013, value_2014, value_2015, value_2016, value_2017, na.rm = T), average = mean(x = c(value_2012, value_2013, value_2014, value_2015, value_2016, value_2017), na.rm = T))

states_mass_total <- states_mass %>%
  dplyr::select(-sum, -average) %>%
  group_by(origin, destination) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  group_by(origin, destination) %>%
  mutate(sum = sum(mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017, na.rm = T), average = mean(x = c(mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017), na.rm = T))

##########################################################################################################################################
#read in country trade with states
countries <- read_csv("./USCensus_trade_countries_13_june_2019.csv")

#clean up col labels and order, then get some summary data
countries <- countries %>%
  mutate(origin = State, destination = Country, value = `Total Value ($US)`) %>%
  dplyr::select(-Commodity, -State, -Country, -`Total Value ($US)`) %>%
  dplyr::select(origin, destination, Time:value) %>%
  group_by(origin, destination, Time) %>%
  mutate(mass = sum(`Vessel SWT (kg)`, `Containerized Vessel Total Exports SWT (kg)`, `Air SWT (kg)`, na.rm = T), year = Time) %>%
  group_by(origin, destination) %>%
  dplyr::select(origin, destination, year, value, mass, -Time)
 
#create a summary table for countries that matches states

#mass
countries_mass <- countries %>%
  dplyr::select(-value) %>%
  group_by(origin, destination) %>%
  spread(value = mass, key = year)

colnames(countries_mass) <- gsub(pattern = "201", replacement = "mass_201", x = colnames(countries_mass))

#get summary stats across years
countries_mass <- countries_mass %>%
  group_by(origin, destination) %>%
  mutate(sum = sum(mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017, na.rm = T), average = mean(x = c(mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017), na.rm = T))


#value
countries_value <- countries %>%
  dplyr::select(-mass) %>%
  group_by(origin, destination) %>%
  spread(value = value, key = year)

colnames(countries_value) <- gsub(pattern = "201", replacement = "value_201", x = colnames(countries_value))

#get summary stats across years
countries_value <- countries_value %>%
  group_by(origin, destination) %>%
  mutate(sum = sum(value_2012, value_2013, value_2014, value_2015, value_2016, value_2017, na.rm = T), average = mean(x = c(value_2012, value_2013, value_2014, value_2015, value_2016, value_2017), na.rm = T))


##########################################################################################################################################
#write out the new CSV's for use

#countries
  #mass
write_csv(x = countries_mass, path = "./FINAL_DATA/countries_mass_14_june_2019.csv")
  #value
write_csv(x = countries_value, path = "./FINAL_DATA/countries_value_14_june_2019.csv")

#state
  #mass
write_csv(x = states_mass_total, path = "./FINAL_DATA/states_mass_14_june_2019.csv")
  #value
write_csv(x = states_total, path = "./FINAL_DATA/states_value_14_june_2019.csv")


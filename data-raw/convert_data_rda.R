#script to convert raw data to .RDA form and put in the data directory of package

#Authors: NAH

#load required packages
library(tidyverse)
library(usethis)
library(tools)
library(raster)
library(here)
library(RStoolbox)

####################################################################################################################
#data to read in and clean
####################################################################################################################

##########################################################
#wine market risk
##########################################################

#COUNTRIES
countries_market <- read_csv("./data-raw/wine_market_FAOSTAT_data_8-31-2020.csv")


#rm NA's
countries_market <- countries_market %>%
  filter(!is.na(Value))

#recode some country names and make sure that Area == ID
countries_market <- countries_market %>%
  mutate(ID = Area) %>%
  mutate(
    ID = recode(
      ID,
      `Bolivia (Plurinational State of)` = "Bolivia",
      `Brunei Darussalam` = "Brunei",
      `Cabo Verde` = "Cape Verde",
      `China, mainland` = "China",
      Congo = "Republic of Congo",
      `Falkland Islands (Malvinas)` = "Falkland Islands",
      `China, Hong Kong SAR` = "Hong Kong",
      `Iran (Islamic Republic of)` = "Iran",
      `Côte d'Ivoire` = "Ivory Coast",
      `Democratic People's Republic of Korea` = "North Korea",
      `Republic of Korea` = "South Korea",
      `Lao People's Democratic Republic` = "Laos",
      `China, Macao SAR` = "Macao",
      `Republic of Moldova` = "Moldova",
      `Netherlands Antilles (former)` = "Bonaire, Sint Eustatius and Saba",
      `North Macedonia` = "Macedonia",
      Czechia = "Czech Republic",
      `Russian Federation` = "Russia",
      `Syrian Arab Republic`  = "Syria",
      `China, Taiwan Province of` = "Taiwan",
      `United Republic of Tanzania` = "Tanzania",
      `United Kingdom of Great Britain and Northern Ireland` = "United Kingdom",
      `United States of America` = "United States",
      `Venezuela (Bolivarian Republic of)` = "Venezuela",
      `Viet Nam` = "Vietnam",
      Palestine = "Palestina",
      Eswatini = "Swaziland"

    )
  ) %>%
  mutate(Area = ID)


##########################################################
#correlation plots
##########################################################

#############################
  #trade
#############################

#Trade data inherently requires cleaning and tidying from the source

#STATES
states <- read_csv("./data-raw/FAFdata_states_13_june_2019.csv")
states_mass <- read_csv("./data-raw/FAFdata_states_tons_13_june_2019.csv")

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

#now we take the overall commerce and summarize for every pair of origin and destination
#value
states_value <- states %>%
  dplyr::select(-sum, -average) %>%
  group_by(origin, destination) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  group_by(origin, destination) %>%
  mutate(sum = sum(value_2012, value_2013, value_2014, value_2015, value_2016, value_2017, na.rm = T), average = mean(x = c(value_2012, value_2013, value_2014, value_2015, value_2016, value_2017), na.rm = T))

#same for mass
states_mass <- states_mass %>%
  dplyr::select(-sum, -average) %>%
  group_by(origin, destination) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  group_by(origin, destination) %>%
  mutate(sum = sum(mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017, na.rm = T), average = mean(x = c(mass_2012, mass_2013, mass_2014, mass_2015, mass_2016, mass_2017), na.rm = T))

#filter to remove in state commerce (origin == destination)
states_value <- states_value %>%
  filter(!destination == origin)
states_mass <- states_mass %>%
  filter(!destination == origin)

#trade VALUE
{#first all trade by state
#this gets totals and averages per destination across years and for individual years
trade_summary <- states_value %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(all_total = sum(sum, na.rm = T),
            all_average = (sum(sum, na.rm = T) / 6),
            all_2012 = sum(value_2012, na.rm = T),
            all_2013 = sum(value_2013, na.rm = T),
            all_2014 = sum(value_2014, na.rm = T),
            all_2015 = sum(value_2015, na.rm = T),
            all_2016 = sum(value_2016, na.rm = T),
            all_2017 = sum(value_2017, na.rm = T)
  )

#Delaware
trade_de <- states_value %>%
  filter(origin %in% "Delaware") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(de_total = sum(sum, na.rm = T),
            de_average = (sum(sum, na.rm = T) / 6),
            de_2012 = sum(value_2012, na.rm = T),
            de_2013 = sum(value_2013, na.rm = T),
            de_2014 = sum(value_2014, na.rm = T),
            de_2015 = sum(value_2015, na.rm = T),
            de_2016 = sum(value_2016, na.rm = T),
            de_2017 = sum(value_2017, na.rm = T)
  )

#Maryland
trade_md <- states_value %>%
  filter(origin %in% "Maryland") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(md_total = sum(sum, na.rm = T),
            md_average = (sum(sum, na.rm = T) / 6),
            md_2012 = sum(value_2012, na.rm = T),
            md_2013 = sum(value_2013, na.rm = T),
            md_2014 = sum(value_2014, na.rm = T),
            md_2015 = sum(value_2015, na.rm = T),
            md_2016 = sum(value_2016, na.rm = T),
            md_2017 = sum(value_2017, na.rm = T)
  )

#New Jersey
trade_nj <- states_value %>%
  filter(origin %in% "New Jersey") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(nj_total = sum(sum, na.rm = T),
            nj_average = (sum(sum, na.rm = T) / 6),
            nj_2012 = sum(value_2012, na.rm = T),
            nj_2013 = sum(value_2013, na.rm = T),
            nj_2014 = sum(value_2014, na.rm = T),
            nj_2015 = sum(value_2015, na.rm = T),
            nj_2016 = sum(value_2016, na.rm = T),
            nj_2017 = sum(value_2017, na.rm = T)
  )

#pull data for states with PA specifically as the origin
trade_pa <- states_value %>%
  filter(origin %in% "Pennsylvania") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(pa_total = sum(sum, na.rm = T),
            pa_average = (sum(sum, na.rm = T) / 6),
            pa_2012 = sum(value_2012, na.rm = T),
            pa_2013 = sum(value_2013, na.rm = T),
            pa_2014 = sum(value_2014, na.rm = T),
            pa_2015 = sum(value_2015, na.rm = T),
            pa_2016 = sum(value_2016, na.rm = T),
            pa_2017 = sum(value_2017, na.rm = T)
  )

#Virginia
trade_va <- states_value %>%
  filter(origin %in% "Virginia") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(va_total = sum(sum, na.rm = T),
            va_average = (sum(sum, na.rm = T) / 6),
            va_2012 = sum(value_2012, na.rm = T),
            va_2013 = sum(value_2013, na.rm = T),
            va_2014 = sum(value_2014, na.rm = T),
            va_2015 = sum(value_2015, na.rm = T),
            va_2016 = sum(value_2016, na.rm = T),
            va_2017 = sum(value_2017, na.rm = T)
  )

#North Carolina
trade_nc <- states_value %>%
  filter(origin %in% "North Carolina") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(nc_total = sum(sum, na.rm = T),
            nc_average = (sum(sum, na.rm = T) / 6),
            nc_2012 = sum(value_2012, na.rm = T),
            nc_2013 = sum(value_2013, na.rm = T),
            nc_2014 = sum(value_2014, na.rm = T),
            nc_2015 = sum(value_2015, na.rm = T),
            nc_2016 = sum(value_2016, na.rm = T),
            nc_2017 = sum(value_2017, na.rm = T)
  )

#New York
trade_ny <- states_value %>%
  filter(origin %in% "New York") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(ny_total = sum(sum, na.rm = T),
            ny_average = (sum(sum, na.rm = T) / 6),
            ny_2012 = sum(value_2012, na.rm = T),
            ny_2013 = sum(value_2013, na.rm = T),
            ny_2014 = sum(value_2014, na.rm = T),
            ny_2015 = sum(value_2015, na.rm = T),
            ny_2016 = sum(value_2016, na.rm = T),
            ny_2017 = sum(value_2017, na.rm = T)
  )

#Ohio
trade_oh <- states_value %>%
  filter(origin %in% "Ohio") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(oh_total = sum(sum, na.rm = T),
            oh_average = (sum(sum, na.rm = T) / 6),
            oh_2012 = sum(value_2012, na.rm = T),
            oh_2013 = sum(value_2013, na.rm = T),
            oh_2014 = sum(value_2014, na.rm = T),
            oh_2015 = sum(value_2015, na.rm = T),
            oh_2016 = sum(value_2016, na.rm = T),
            oh_2017 = sum(value_2017, na.rm = T)
  )

#West Virginia
trade_wv <- states_value %>%
  filter(origin %in% "West Virginia") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(wv_total = sum(sum, na.rm = T),
            wv_average = (sum(sum, na.rm = T) / 6),
            wv_2012 = sum(value_2012, na.rm = T),
            wv_2013 = sum(value_2013, na.rm = T),
            wv_2014 = sum(value_2014, na.rm = T),
            wv_2015 = sum(value_2015, na.rm = T),
            wv_2016 = sum(value_2016, na.rm = T),
            wv_2017 = sum(value_2017, na.rm = T)
  )
}

###PRESENT TRADE VALUE
#add all infected state data added to new summary object
trade_summary_slf <- left_join(trade_summary, trade_pa, by = "destination") %>%
  left_join(., trade_de, by = "destination") %>%
  left_join(., trade_md, by = "destination") %>%
  left_join(., trade_nj, by = "destination") %>%
  left_join(., trade_va, by = "destination")

#add info for the total of all infected states
trade_summary_slf <- trade_summary_slf %>%
  group_by(destination) %>%
  mutate(
    infected_total = sum(c(pa_total, de_total, md_total, nj_total, va_total), na.rm = T),
    infected_average = sum(c(pa_total, de_total, md_total, nj_total, va_total), na.rm = T)/30 #30 is 6 years * 5 total infected states, previous average removes NA's, forgetting them in the mean calc
  )


###FUTURE TRADE VALUE
#new version of summarizing that works for new states
#add all infected state data added to new summary object
trade_summary_slf_future <- left_join(trade_summary, trade_pa, by = "destination") %>%
  left_join(., trade_de, by = "destination") %>%
  left_join(., trade_md, by = "destination") %>%
  left_join(., trade_nc, by = "destination") %>%
  left_join(., trade_nj, by = "destination") %>%
  left_join(., trade_ny, by = "destination") %>%
  left_join(., trade_oh, by = "destination") %>%
  left_join(., trade_va, by = "destination") %>%
  left_join(., trade_wv, by = "destination")

#add info for the total of all infected states
trade_summary_slf_future <- trade_summary_slf_future %>%
  group_by(destination) %>%
  mutate(
    infected_total = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T),
    infected_average = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T)/54 #54 is 6 years * 9 total infected states, previous average removes NA's, forgetting them in the mean calc
  )


#replace the Na's with zeros
trade_summary_slf[is.na(trade_summary_slf)] <- 0
trade_summary_slf_future[is.na(trade_summary_slf_future)] <- 0

#change the name of DC
trade_summary_slf$destination <- gsub(pattern = "Washington DC", replacement = "District of Columbia", x = trade_summary_slf$destination)
trade_summary_slf_future$destination <- gsub(pattern = "Washington DC", replacement = "District of Columbia", x = trade_summary_slf_future$destination)

#change the names to work
states_trade_summary_slf <- trade_summary_slf
states_trade_summary_slf_future <- trade_summary_slf_future

#trade MASS
{#first all trade by state
#this gets totals and averages per destination across years and for individual years
trade_mass_summary <- states_mass %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(all_total = sum(sum, na.rm = T),
            all_average = (sum(sum, na.rm = T) / 6),
            all_2012 = sum(mass_2012, na.rm = T),
            all_2013 = sum(mass_2013, na.rm = T),
            all_2014 = sum(mass_2014, na.rm = T),
            all_2015 = sum(mass_2015, na.rm = T),
            all_2016 = sum(mass_2016, na.rm = T),
            all_2017 = sum(mass_2017, na.rm = T)
  )

#Delaware
trade_mass_de <- states_mass %>%
  filter(origin %in% "Delaware") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(de_total = sum(sum, na.rm = T),
            de_average = (sum(sum, na.rm = T) / 6),
            de_2012 = sum(mass_2012, na.rm = T),
            de_2013 = sum(mass_2013, na.rm = T),
            de_2014 = sum(mass_2014, na.rm = T),
            de_2015 = sum(mass_2015, na.rm = T),
            de_2016 = sum(mass_2016, na.rm = T),
            de_2017 = sum(mass_2017, na.rm = T)
  )

#Maryland
trade_mass_md <- states_mass %>%
  filter(origin %in% "Maryland") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(md_total = sum(sum, na.rm = T),
            md_average = (sum(sum, na.rm = T) / 6),
            md_2012 = sum(mass_2012, na.rm = T),
            md_2013 = sum(mass_2013, na.rm = T),
            md_2014 = sum(mass_2014, na.rm = T),
            md_2015 = sum(mass_2015, na.rm = T),
            md_2016 = sum(mass_2016, na.rm = T),
            md_2017 = sum(mass_2017, na.rm = T)
  )

#New Jersey
trade_mass_nj <- states_mass %>%
  filter(origin %in% "New Jersey") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(nj_total = sum(sum, na.rm = T),
            nj_average = (sum(sum, na.rm = T) / 6),
            nj_2012 = sum(mass_2012, na.rm = T),
            nj_2013 = sum(mass_2013, na.rm = T),
            nj_2014 = sum(mass_2014, na.rm = T),
            nj_2015 = sum(mass_2015, na.rm = T),
            nj_2016 = sum(mass_2016, na.rm = T),
            nj_2017 = sum(mass_2017, na.rm = T)
  )

#pull data for states with PA specifically as the origin
trade_mass_pa <- states_mass %>%
  filter(origin %in% "Pennsylvania") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(pa_total = sum(sum, na.rm = T),
            pa_average = (sum(sum, na.rm = T) / 6),
            pa_2012 = sum(mass_2012, na.rm = T),
            pa_2013 = sum(mass_2013, na.rm = T),
            pa_2014 = sum(mass_2014, na.rm = T),
            pa_2015 = sum(mass_2015, na.rm = T),
            pa_2016 = sum(mass_2016, na.rm = T),
            pa_2017 = sum(mass_2017, na.rm = T)
  )

#Virginia
trade_mass_va <- states_mass %>%
  filter(origin %in% "Virginia") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(va_total = sum(sum, na.rm = T),
            va_average = (sum(sum, na.rm = T) / 6),
            va_2012 = sum(mass_2012, na.rm = T),
            va_2013 = sum(mass_2013, na.rm = T),
            va_2014 = sum(mass_2014, na.rm = T),
            va_2015 = sum(mass_2015, na.rm = T),
            va_2016 = sum(mass_2016, na.rm = T),
            va_2017 = sum(mass_2017, na.rm = T)
  )

#North Carolina
trade_mass_nc <- states_mass %>%
  filter(origin %in% "North Carolina") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(nc_total = sum(sum, na.rm = T),
            nc_average = (sum(sum, na.rm = T) / 6),
            nc_2012 = sum(mass_2012, na.rm = T),
            nc_2013 = sum(mass_2013, na.rm = T),
            nc_2014 = sum(mass_2014, na.rm = T),
            nc_2015 = sum(mass_2015, na.rm = T),
            nc_2016 = sum(mass_2016, na.rm = T),
            nc_2017 = sum(mass_2017, na.rm = T)
  )

#New York
trade_mass_ny <- states_mass %>%
  filter(origin %in% "New York") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(ny_total = sum(sum, na.rm = T),
            ny_average = (sum(sum, na.rm = T) / 6),
            ny_2012 = sum(mass_2012, na.rm = T),
            ny_2013 = sum(mass_2013, na.rm = T),
            ny_2014 = sum(mass_2014, na.rm = T),
            ny_2015 = sum(mass_2015, na.rm = T),
            ny_2016 = sum(mass_2016, na.rm = T),
            ny_2017 = sum(mass_2017, na.rm = T)
  )

#Ohio
trade_mass_oh <- states_mass %>%
  filter(origin %in% "Ohio") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(oh_total = sum(sum, na.rm = T),
            oh_average = (sum(sum, na.rm = T) / 6),
            oh_2012 = sum(mass_2012, na.rm = T),
            oh_2013 = sum(mass_2013, na.rm = T),
            oh_2014 = sum(mass_2014, na.rm = T),
            oh_2015 = sum(mass_2015, na.rm = T),
            oh_2016 = sum(mass_2016, na.rm = T),
            oh_2017 = sum(mass_2017, na.rm = T)
  )

#West Virginia
trade_mass_wv <- states_mass %>%
  filter(origin %in% "West Virginia") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(wv_total = sum(sum, na.rm = T),
            wv_average = (sum(sum, na.rm = T) / 6),
            wv_2012 = sum(mass_2012, na.rm = T),
            wv_2013 = sum(mass_2013, na.rm = T),
            wv_2014 = sum(mass_2014, na.rm = T),
            wv_2015 = sum(mass_2015, na.rm = T),
            wv_2016 = sum(mass_2016, na.rm = T),
            wv_2017 = sum(mass_2017, na.rm = T)
  )
}

###PRESENT TRADE MASS
#add all infected state data added to new summary object
trade_mass_summary_slf <- left_join(trade_mass_summary, trade_mass_pa, by = "destination") %>%
  left_join(., trade_mass_de, by = "destination") %>%
  left_join(., trade_mass_md, by = "destination") %>%
  left_join(., trade_mass_nj, by = "destination") %>%
  left_join(., trade_mass_va, by = "destination")

#add info for the total of all infected states
trade_mass_summary_slf <- trade_mass_summary_slf %>%
  group_by(destination) %>%
  mutate(
    infected_total = sum(c(pa_total, de_total, md_total, nj_total, va_total), na.rm = T),
    infected_average = sum(c(pa_total, de_total, md_total, nj_total, va_total), na.rm = T)/30 #30 is 6 years * 5 total infected states, previous average removes NA's, forgetting them in the mean calc
  )

###FUTURE TRADE MASS
#new version of summarizing that works for new states
#add all infected state data added to new summary object
trade_mass_summary_slf_future <- left_join(trade_mass_summary, trade_mass_pa, by = "destination") %>%
  left_join(., trade_mass_de, by = "destination") %>%
  left_join(., trade_mass_md, by = "destination") %>%
  left_join(., trade_mass_nc, by = "destination") %>%
  left_join(., trade_mass_nj, by = "destination") %>%
  left_join(., trade_mass_ny, by = "destination") %>%
  left_join(., trade_mass_oh, by = "destination") %>%
  left_join(., trade_mass_va, by = "destination") %>%
  left_join(., trade_mass_wv, by = "destination")

#add info for the total of all infected states
trade_mass_summary_slf_future <- trade_mass_summary_slf_future %>%
  group_by(destination) %>%
  mutate(
    infected_total = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T),
    infected_average = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T)/54 #54 is 6 years * 9 total infected states, previous average removes NA's, forgetting them in the mean calc
  )


#replace the Na's with zeros
trade_mass_summary_slf[is.na(trade_mass_summary_slf)] <- 0
trade_mass_summary_slf_future[is.na(trade_mass_summary_slf_future)] <- 0

#change the name of DC
trade_mass_summary_slf$destination <- gsub(pattern = "Washington DC", replacement = "District of Columbia", x = trade_mass_summary_slf$destination)
trade_mass_summary_slf_future$destination <- gsub(pattern = "Washington DC", replacement = "District of Columbia", x = trade_mass_summary_slf_future$destination)

#rename to make naming  convention work better
states_trade_mass_summary_slf <- trade_mass_summary_slf
states_trade_mass_summary_slf_future <- trade_mass_summary_slf_future

#COUNTRIES
countries <- read_csv("./data-raw/USCensus_trade_countries_13_june_2019.csv")

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


#first all trade by state
#this gets totals and averages per destination across years and for individual years
#trade VALUE
{
trade_summary <- countries_value %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(all_total = sum(sum, na.rm = T),
            all_average = (sum(sum, na.rm = T) / 6),
            all_2012 = sum(value_2012, na.rm = T),
            all_2013 = sum(value_2013, na.rm = T),
            all_2014 = sum(value_2014, na.rm = T),
            all_2015 = sum(value_2015, na.rm = T),
            all_2016 = sum(value_2016, na.rm = T),
            all_2017 = sum(value_2017, na.rm = T)
  )

#Delaware
trade_de <- countries_value %>%
  filter(origin %in% "Delaware") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(de_total = sum(sum, na.rm = T),
            de_average = (sum(sum, na.rm = T) / 6),
            de_2012 = sum(value_2012, na.rm = T),
            de_2013 = sum(value_2013, na.rm = T),
            de_2014 = sum(value_2014, na.rm = T),
            de_2015 = sum(value_2015, na.rm = T),
            de_2016 = sum(value_2016, na.rm = T),
            de_2017 = sum(value_2017, na.rm = T)
  )

#Maryland
trade_md <- countries_value %>%
  filter(origin %in% "Maryland") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(md_total = sum(sum, na.rm = T),
            md_average = (sum(sum, na.rm = T) / 6),
            md_2012 = sum(value_2012, na.rm = T),
            md_2013 = sum(value_2013, na.rm = T),
            md_2014 = sum(value_2014, na.rm = T),
            md_2015 = sum(value_2015, na.rm = T),
            md_2016 = sum(value_2016, na.rm = T),
            md_2017 = sum(value_2017, na.rm = T)
  )

#New Jersey
trade_nj <- countries_value %>%
  filter(origin %in% "New Jersey") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(nj_total = sum(sum, na.rm = T),
            nj_average = (sum(sum, na.rm = T) / 6),
            nj_2012 = sum(value_2012, na.rm = T),
            nj_2013 = sum(value_2013, na.rm = T),
            nj_2014 = sum(value_2014, na.rm = T),
            nj_2015 = sum(value_2015, na.rm = T),
            nj_2016 = sum(value_2016, na.rm = T),
            nj_2017 = sum(value_2017, na.rm = T)
  )

#pull data for states with PA specifically as the origin
trade_pa <- countries_value %>%
  filter(origin %in% "Pennsylvania") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(pa_total = sum(sum, na.rm = T),
            pa_average = (sum(sum, na.rm = T) / 6),
            pa_2012 = sum(value_2012, na.rm = T),
            pa_2013 = sum(value_2013, na.rm = T),
            pa_2014 = sum(value_2014, na.rm = T),
            pa_2015 = sum(value_2015, na.rm = T),
            pa_2016 = sum(value_2016, na.rm = T),
            pa_2017 = sum(value_2017, na.rm = T)
  )

#Virginia
trade_va <- countries_value %>%
  filter(origin %in% "Virginia") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(va_total = sum(sum, na.rm = T),
            va_average = (sum(sum, na.rm = T) / 6),
            va_2012 = sum(value_2012, na.rm = T),
            va_2013 = sum(value_2013, na.rm = T),
            va_2014 = sum(value_2014, na.rm = T),
            va_2015 = sum(value_2015, na.rm = T),
            va_2016 = sum(value_2016, na.rm = T),
            va_2017 = sum(value_2017, na.rm = T)
  )

#North Carolina
trade_nc <- countries_value %>%
  filter(origin %in% "North Carolina") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(nc_total = sum(sum, na.rm = T),
            nc_average = (sum(sum, na.rm = T) / 6),
            nc_2012 = sum(value_2012, na.rm = T),
            nc_2013 = sum(value_2013, na.rm = T),
            nc_2014 = sum(value_2014, na.rm = T),
            nc_2015 = sum(value_2015, na.rm = T),
            nc_2016 = sum(value_2016, na.rm = T),
            nc_2017 = sum(value_2017, na.rm = T)
  )

#New York
trade_ny <- countries_value %>%
  filter(origin %in% "New York") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(ny_total = sum(sum, na.rm = T),
            ny_average = (sum(sum, na.rm = T) / 6),
            ny_2012 = sum(value_2012, na.rm = T),
            ny_2013 = sum(value_2013, na.rm = T),
            ny_2014 = sum(value_2014, na.rm = T),
            ny_2015 = sum(value_2015, na.rm = T),
            ny_2016 = sum(value_2016, na.rm = T),
            ny_2017 = sum(value_2017, na.rm = T)
  )

#Ohio
trade_oh <- countries_value %>%
  filter(origin %in% "Ohio") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(oh_total = sum(sum, na.rm = T),
            oh_average = (sum(sum, na.rm = T) / 6),
            oh_2012 = sum(value_2012, na.rm = T),
            oh_2013 = sum(value_2013, na.rm = T),
            oh_2014 = sum(value_2014, na.rm = T),
            oh_2015 = sum(value_2015, na.rm = T),
            oh_2016 = sum(value_2016, na.rm = T),
            oh_2017 = sum(value_2017, na.rm = T)
  )

#West Virginia
trade_wv <- countries_value %>%
  filter(origin %in% "West Virginia") %>%
  group_by(destination) %>%
  arrange(destination) %>%
  summarize(wv_total = sum(sum, na.rm = T),
            wv_average = (sum(sum, na.rm = T) / 6),
            wv_2012 = sum(value_2012, na.rm = T),
            wv_2013 = sum(value_2013, na.rm = T),
            wv_2014 = sum(value_2014, na.rm = T),
            wv_2015 = sum(value_2015, na.rm = T),
            wv_2016 = sum(value_2016, na.rm = T),
            wv_2017 = sum(value_2017, na.rm = T)
  )
}

###PRESENT TRADE VALUE
#add all infected state data added to new summary object
countries_trade_summary_slf <- left_join(trade_summary, trade_pa, by = "destination") %>%
  left_join(., trade_de, by = "destination") %>%
  left_join(., trade_md, by = "destination") %>%
  left_join(., trade_nj, by = "destination") %>%
  left_join(., trade_va, by = "destination")

#add info for the total of all infected states
countries_trade_summary_slf <- countries_trade_summary_slf %>%
  group_by(destination) %>%
  mutate(
    infected_total = sum(c(pa_total, de_total, md_total, nj_total, va_total), na.rm = T),
    infected_average = sum(c(pa_total, de_total, md_total, nj_total, va_total), na.rm = T)/30 #30 is 6 years * 5 total infected states, previous average removes NA's, forgetting them in the mean calc
  )

###FUTURE TRADE VALUE
#new version of summarizing that works for new states
#add all infected state data added to new summary object
countries_trade_summary_slf_future <- left_join(trade_summary, trade_pa, by = "destination") %>%
  left_join(., trade_de, by = "destination") %>%
  left_join(., trade_md, by = "destination") %>%
  left_join(., trade_nc, by = "destination") %>%
  left_join(., trade_nj, by = "destination") %>%
  left_join(., trade_ny, by = "destination") %>%
  left_join(., trade_oh, by = "destination") %>%
  left_join(., trade_va, by = "destination") %>%
  left_join(., trade_wv, by = "destination")

#add info for the total of all infected states
countries_trade_summary_slf_future <- countries_trade_summary_slf_future %>%
  group_by(destination) %>%
  mutate(
    infected_total = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T),
    infected_average = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T)/54 #54 is 6 years * 9 total infected states, previous average removes NA's, forgetting them in the mean calc
  )


#replace the Na's with zeros
countries_trade_summary_slf[is.na(countries_trade_summary_slf)] <- 0
countries_trade_summary_slf_future[is.na(countries_trade_summary_slf_future)] <- 0


#trade MASS
{
  trade_mass_summary <- countries_mass %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(all_total = sum(sum, na.rm = T),
              all_average = (sum(sum, na.rm = T) / 6),
              all_2012 = sum(mass_2012, na.rm = T),
              all_2013 = sum(mass_2013, na.rm = T),
              all_2014 = sum(mass_2014, na.rm = T),
              all_2015 = sum(mass_2015, na.rm = T),
              all_2016 = sum(mass_2016, na.rm = T),
              all_2017 = sum(mass_2017, na.rm = T)
    )

  #Delaware
  trade_mass_de <- countries_mass %>%
    filter(origin %in% "Delaware") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(de_total = sum(sum, na.rm = T),
              de_average = (sum(sum, na.rm = T) / 6),
              de_2012 = sum(mass_2012, na.rm = T),
              de_2013 = sum(mass_2013, na.rm = T),
              de_2014 = sum(mass_2014, na.rm = T),
              de_2015 = sum(mass_2015, na.rm = T),
              de_2016 = sum(mass_2016, na.rm = T),
              de_2017 = sum(mass_2017, na.rm = T)
    )

  #Maryland
  trade_mass_md <- countries_mass %>%
    filter(origin %in% "Maryland") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(md_total = sum(sum, na.rm = T),
              md_average = (sum(sum, na.rm = T) / 6),
              md_2012 = sum(mass_2012, na.rm = T),
              md_2013 = sum(mass_2013, na.rm = T),
              md_2014 = sum(mass_2014, na.rm = T),
              md_2015 = sum(mass_2015, na.rm = T),
              md_2016 = sum(mass_2016, na.rm = T),
              md_2017 = sum(mass_2017, na.rm = T)
    )

  #New Jersey
  trade_mass_nj <- countries_mass %>%
    filter(origin %in% "New Jersey") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(nj_total = sum(sum, na.rm = T),
              nj_average = (sum(sum, na.rm = T) / 6),
              nj_2012 = sum(mass_2012, na.rm = T),
              nj_2013 = sum(mass_2013, na.rm = T),
              nj_2014 = sum(mass_2014, na.rm = T),
              nj_2015 = sum(mass_2015, na.rm = T),
              nj_2016 = sum(mass_2016, na.rm = T),
              nj_2017 = sum(mass_2017, na.rm = T)
    )

  #pull data for countries with PA specifically as the origin
  trade_mass_pa <- countries_mass %>%
    filter(origin %in% "Pennsylvania") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(pa_total = sum(sum, na.rm = T),
              pa_average = (sum(sum, na.rm = T) / 6),
              pa_2012 = sum(mass_2012, na.rm = T),
              pa_2013 = sum(mass_2013, na.rm = T),
              pa_2014 = sum(mass_2014, na.rm = T),
              pa_2015 = sum(mass_2015, na.rm = T),
              pa_2016 = sum(mass_2016, na.rm = T),
              pa_2017 = sum(mass_2017, na.rm = T)
    )

  #Virginia
  trade_mass_va <- countries_mass %>%
    filter(origin %in% "Virginia") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(va_total = sum(sum, na.rm = T),
              va_average = (sum(sum, na.rm = T) / 6),
              va_2012 = sum(mass_2012, na.rm = T),
              va_2013 = sum(mass_2013, na.rm = T),
              va_2014 = sum(mass_2014, na.rm = T),
              va_2015 = sum(mass_2015, na.rm = T),
              va_2016 = sum(mass_2016, na.rm = T),
              va_2017 = sum(mass_2017, na.rm = T)
    )

  #North Carolina
  trade_mass_nc <- countries_mass %>%
    filter(origin %in% "North Carolina") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(nc_total = sum(sum, na.rm = T),
              nc_average = (sum(sum, na.rm = T) / 6),
              nc_2012 = sum(mass_2012, na.rm = T),
              nc_2013 = sum(mass_2013, na.rm = T),
              nc_2014 = sum(mass_2014, na.rm = T),
              nc_2015 = sum(mass_2015, na.rm = T),
              nc_2016 = sum(mass_2016, na.rm = T),
              nc_2017 = sum(mass_2017, na.rm = T)
    )

  #New York
  trade_mass_ny <- countries_mass %>%
    filter(origin %in% "New York") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(ny_total = sum(sum, na.rm = T),
              ny_average = (sum(sum, na.rm = T) / 6),
              ny_2012 = sum(mass_2012, na.rm = T),
              ny_2013 = sum(mass_2013, na.rm = T),
              ny_2014 = sum(mass_2014, na.rm = T),
              ny_2015 = sum(mass_2015, na.rm = T),
              ny_2016 = sum(mass_2016, na.rm = T),
              ny_2017 = sum(mass_2017, na.rm = T)
    )

  #Ohio
  trade_mass_oh <- countries_mass %>%
    filter(origin %in% "Ohio") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(oh_total = sum(sum, na.rm = T),
              oh_average = (sum(sum, na.rm = T) / 6),
              oh_2012 = sum(mass_2012, na.rm = T),
              oh_2013 = sum(mass_2013, na.rm = T),
              oh_2014 = sum(mass_2014, na.rm = T),
              oh_2015 = sum(mass_2015, na.rm = T),
              oh_2016 = sum(mass_2016, na.rm = T),
              oh_2017 = sum(mass_2017, na.rm = T)
    )

  #West Virginia
  trade_mass_wv <- countries_mass %>%
    filter(origin %in% "West Virginia") %>%
    group_by(destination) %>%
    arrange(destination) %>%
    summarize(wv_total = sum(sum, na.rm = T),
              wv_average = (sum(sum, na.rm = T) / 6),
              wv_2012 = sum(mass_2012, na.rm = T),
              wv_2013 = sum(mass_2013, na.rm = T),
              wv_2014 = sum(mass_2014, na.rm = T),
              wv_2015 = sum(mass_2015, na.rm = T),
              wv_2016 = sum(mass_2016, na.rm = T),
              wv_2017 = sum(mass_2017, na.rm = T)
    )
}


#PRESENT TRADE MASS
#add all infected state data added to new summary object
countries_trade_mass_summary_slf <- left_join(trade_mass_summary, trade_mass_pa, by = "destination") %>%
  left_join(., trade_mass_de, by = "destination") %>%
  left_join(., trade_mass_md, by = "destination") %>%
  left_join(., trade_mass_nj, by = "destination") %>%
  left_join(., trade_mass_va, by = "destination")

#add info for the total of all infected states
countries_trade_mass_summary_slf <- countries_trade_mass_summary_slf %>%
  group_by(destination) %>%
  mutate(
    infected_total = sum(c(pa_total, de_total, md_total, nj_total, va_total), na.rm = T),
    infected_average = sum(c(pa_total, de_total, md_total, nj_total, va_total), na.rm = T)/30 #30 is 6 years * 5 total infected states, previous average removes NA's, forgetting them in the mean calc
  )

#FUTURE TRADE MASS
countries_trade_mass_summary_slf_future <- left_join(trade_mass_summary, trade_mass_pa, by = "destination") %>%
  left_join(., trade_mass_de, by = "destination") %>%
  left_join(., trade_mass_md, by = "destination") %>%
  left_join(., trade_mass_nc, by = "destination") %>%
  left_join(., trade_mass_nj, by = "destination") %>%
  left_join(., trade_mass_ny, by = "destination") %>%
  left_join(., trade_mass_oh, by = "destination") %>%
  left_join(., trade_mass_va, by = "destination") %>%
  left_join(., trade_mass_wv, by = "destination")

#add info for the total of all infected states
countries_trade_mass_summary_slf_future <- countries_trade_mass_summary_slf_future %>%
  group_by(destination) %>%
  mutate(
    infected_total = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T),
    infected_average = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T)/54 #54 is 6 years * 9 total infected states, previous average removes NA's, forgetting them in the mean calc
  )

#replace the Na's with zeros
countries_trade_mass_summary_slf[is.na(countries_trade_mass_summary_slf)] <- 0
countries_trade_mass_summary_slf_future[is.na(countries_trade_mass_summary_slf_future)] <- 0










#############################
  #grapes
#############################
#STATES
#yield
#(convert the yield)
states_grapes_yield <- read_csv(file = "./data-raw/states_grape_yield_2012-2017.csv") %>%
  dplyr::select(State, Year, Value, `Data Item`) %>%
  mutate(State = tolower(State)) %>%
  mutate(State = toTitleCase(State)) %>%
  rename(geopol_unit = State) %>%  #rename Area to geopol_unit
  mutate(Value = Value*2.2417, Unit = "tonnes/ha") %>%  #convert tons/acre to metric tons/hectare
  mutate(Item = "grape_yield") %>%  #set Item as a description col
  dplyr::select(-`Data Item`) #rm junk col


#production these data run 1998 - 2019
states_grapes_prod <- read_csv(file = "./data-raw/NASS_QUICKSTATS_data__states_grapes_production_01-24-2020.csv") %>%
  dplyr::select(State, Year, Value, `Data Item`) %>%
  mutate(State = tolower(State)) %>%
  mutate(State = toTitleCase(State)) %>%
  rename(geopol_unit = State) %>%  #rename Area to geopol_unit
  filter(Year > 2011 & Year < 2018) %>% #filter out to be 2012-2017
  mutate(Value = as.numeric(gsub(",","", .$Value))) %>% #make the Value numeric and rm commas
  mutate(Value = Value*1.01605, Unit = "tonnes")  %>%  #convert from us tons to metric tonnes
  mutate(Item = "grape_production") %>%  #set Item as a description col
  dplyr::select(-`Data Item`) #rm junk col

#combine yield and production
states_grapes <- bind_rows(states_grapes_yield, states_grapes_prod)


#COUNTRIES
#YIELD
countries_grapes_yield <- read_csv(file = "./data-raw/FAOSTAT_data_4-22-2019.csv") %>%
  dplyr::select(Area, Year, Value, Unit, Item) %>%
  mutate(Item = "grape_yield") %>%  #set Item as a description col
  mutate(Value = Value*0.0001, Unit = "tonnes/ha") %>%   #convert hg/hectare to metric tons/hectare
  mutate(Area = gsub(pattern = "United States of America", replacement = "United States", x = .$Area)) %>% #change the name of United States of America to United States
  rename(geopol_unit = Area)  #rename Area to geopol_unit


#production
countries_grapes_prod <- read_csv("./data-raw/FAOSTAT_data_countries_grapes_production_01-24-2020.csv") %>%
  dplyr::select(Area, Year, Value, Unit, Item) %>%   #select cols of interest
  rename(geopol_unit = Area) %>%   #rename Area to geopol_unit
  mutate(Item = "grape_production") %>%  #set Item as a description col
  filter(Year > 2011 & Year < 2018)   #filter out to be 2012-2017

#combine yield and production
countries_grapes <- bind_rows(countries_grapes_yield, countries_grapes_prod)

#clean up a naming issue
grep(pattern = "Cï¿½te d'Ivoire", x = countries_grapes$geopol_unit) #need to change
countries_grapes$geopol_unit <- gsub(pattern = "Cï¿½te d'Ivoire", replacement = "Ivory Coast", x = countries_grapes$geopol_unit)

#############################
  #wine
#############################
#STATES
states_wine <- read_csv("./data-raw/TTB_data_states_wine.csv")

#clean up the data a little
states_wine <- states_wine %>%
  mutate(geopol_unit = tolower(geopol_unit)) %>%
  mutate(geopol_unit = toTitleCase(geopol_unit))

#convert from gallons to mass in metric tons: conversion on https://www.aqua-calc.com/calculate/food-volume-to-weight#anchor-about
states_wine[,-1] <- states_wine[,-1] * 3.776e-3

#tibble it up
states_wine <- states_wine %>%
  group_by(geopol_unit) %>%
  gather(`2012`:`2017`, key = "Year", value = "Mass") %>%
  mutate(Unit = "tonnes")


#COUNTRIES
countries_wine <- read_csv("./data-raw/FAOSTAT_data_wine_6-21-2019_v2.csv")

#clean up the countries data
countries_wine <- countries_wine %>%
  mutate(geopol_unit = Area, Mass = Value) %>%
  dplyr::select(geopol_unit, Year, Mass, Unit) #%>%
  #mutate(Unit = gsub("tonnes", "tons", x = .$Unit))

#############################
#wineries
#############################

#read in the wineries
wineries <- read_csv(file = "./data-raw/WineList.csv") %>%
  dplyr::rename(y = `Decimal Lat`, x = `Decimal Long`)


#############################
  #suitability models
#############################
    #STATES
states_slf_extract <- read_csv(file = "./data-raw/extract_states_02_15_19_maxent_slf+atc-bio02.csv", col_names = T)
states_toh_extract <- read_csv(file = "./data-raw/extract_states_10_29_18_maxent_toh+atc-bio02.csv", col_names = T)
states_slftoh_extract <- read_csv(file = "./data-raw/extract_states_11_07_18_maxent_slf+toh+atc-bio02.csv", col_names = T)

#add model as a categorical var
states_slf_extract <- states_slf_extract %>%
  mutate(model = "slf")
states_toh_extract <- states_toh_extract %>%
  mutate(model = "toh")
states_slftoh_extract <- states_slftoh_extract %>%
  mutate(model = "slftoh")

#put all of the states model extracts in a single object
states_extracts <- bind_rows(states_slf_extract, states_toh_extract, states_slftoh_extract) %>%
  group_by(geopol_unit) %>%
  arrange(geopol_unit) %>%
  dplyr::select(geopol_unit, model, everything())

#clean up the extract data (change names)
colnames(states_extracts) <- gsub("max", replacement = "obs_max", x = colnames(states_extracts))
colnames(states_extracts) <- gsub("min", replacement = "obs_min", x = colnames(states_extracts))
colnames(states_extracts) <- gsub("mean", replacement = "obs_mean", x = colnames(states_extracts))
colnames(states_extracts) <- gsub("sd", replacement = "obs_sd", x = colnames(states_extracts))


    #COUNTRIES
countries_slf_extract <- read_csv(file = "./data-raw/extract_world_11_07_18_maxent_slf+atc-bio02.csv", col_names = T)
countries_toh_extract <- read_csv(file = "./data-raw/extract_world_11_07_18_maxent_toh+atc-bio02.csv", col_names = T)
countries_slftoh_extract <- read_csv(file = "./data-raw/extract_world_11_07_18_maxent_slf+toh+atc-bio02.csv", col_names = T)

#add model as a categorical var
countries_slf_extract <- countries_slf_extract %>%
  mutate(model = "slf")
countries_toh_extract <- countries_toh_extract %>%
  mutate(model = "toh")
countries_slftoh_extract <- countries_slftoh_extract %>%
  mutate(model = "slftoh")

#put all of the states model extracts in a single object
countries_extracts <- bind_rows(countries_slf_extract, countries_toh_extract, countries_slftoh_extract) %>%
  group_by(geopol_unit) %>%
  arrange(geopol_unit)  %>%
  dplyr::select(geopol_unit, model, everything())

#clean up the extract data (change names)
colnames(countries_extracts) <- gsub("max", replacement = "obs_max", x = colnames(countries_extracts))
colnames(countries_extracts) <- gsub("min", replacement = "obs_min", x = colnames(countries_extracts))
colnames(countries_extracts) <- gsub("mean", replacement = "obs_mean", x = colnames(countries_extracts))
colnames(countries_extracts) <- gsub("sd", replacement = "obs_sd", x = colnames(countries_extracts))

#clean country issues
countries_extracts$geopol_unit[grep(pattern = "Ivoire", x = countries_extracts$geopol_unit, value = F)] <- "Ivory Coast"
countries_extracts$geopol_unit[grep(pattern = "ncipe", x = countries_extracts$geopol_unit, value = F)] <- "Sao Tome and Principe"
countries_extracts$geopol_unit[grep(pattern = "Cura",countries_extracts$geopol_unit, value = F)] <- "Curacao"
countries_extracts$geopol_unit[grep(pattern = "Saint Bart", x = countries_extracts$geopol_unit, value = F)] <- "Saint Barthelemy"

#############################
#suitability models rasters
#############################

#load USA
suitability_usa <- raster("/Volumes/GoogleDrive/Shared drives/slfRiskMapping/data/slfRisk/maxent_models/slftoh_ensemble_usa_downsampled_x4_mean.tif")

#fortify (dataframe-fy) the global model
suitability_usa_df <- fortify(suitability_usa, maxpixels = 1e10)
colnames(suitability_usa_df)[3] <- "value"
#remove the NA values to shrink the object size
suitability_usa_df <- suitability_usa_df[!is.na(suitability_usa_df$value),]

#round the value points
suitability_usa_df$value <- round(suitability_usa_df$value, digits = 2)

#adding the state centers for plotting the map
states_centers <- read_csv(file = "./data-raw/us_capitals.csv")
colnames(states_centers)[1] <- "geopol_unit"

#individual models
slf_usa <- raster("/Volumes/GoogleDrive/Shared drives/slfRiskMapping/data/slfRisk/maxent_models/slf_usa_downsampled_x4.tif")
slftoh_usa <- raster("/Volumes/GoogleDrive/Shared drives/slfRiskMapping/data/slfRisk/maxent_models/slftoh_usa_downsampled_x4.tif")
toh_usa <- raster("/Volumes/GoogleDrive/Shared drives/slfRiskMapping/data/slfRisk/maxent_models/toh_usa_downsampled_x4.tif")


#fortify (dataframe-fy) the global model
slf_usa_df <- fortify(slf_usa, maxpixels = 1e10)
slftoh_usa_df <- fortify(slftoh_usa, maxpixels = 1e10)
toh_usa_df <- fortify(toh_usa, maxpixels = 1e10)

colnames(slf_usa_df)[3] <- "value"
colnames(slftoh_usa_df)[3] <- "value"
colnames(toh_usa_df)[3] <- "value"

#remove the NA values to shrink the object size
slf_usa_df <- slf_usa_df[!is.na(slf_usa_df$value),]
slftoh_usa_df <- slftoh_usa_df[!is.na(slftoh_usa_df$value),]
toh_usa_df <- toh_usa_df[!is.na(toh_usa_df$value),]

#round the value points
slf_usa_df$value <- round(slf_usa_df$value, digits = 2)
slftoh_usa_df$value <- round(slftoh_usa_df$value, digits = 2)
slf_usa_df$value <- round(slf_usa_df$value, digits = 2)


#COUNTRIES
suitability_countries <- raster("/Volumes/GoogleDrive/Shared drives/slfRiskMapping/data/slfRisk/maxent_models/slftoh_ensemble_downsampled_x4_mean.tif")

#fortify (dataframe-fy) the global model
suitability_countries_df <- fortify(suitability_countries, maxpixels = 1e10)
colnames(suitability_countries_df)[3] <- "value"
#remove the NA values to shrink the object size
suitability_countries_df <- suitability_countries_df[!is.na(suitability_countries_df$value),]

#round the value points
suitability_countries_df$value <- round(suitability_countries_df$value, digits = 2)

#individual models
slf <- raster("/Volumes/GoogleDrive/Shared drives/slfRiskMapping/data/slfRisk/maxent_models/slf_downsampled_x10.tif")
slftoh <- raster("/Volumes/GoogleDrive/Shared drives/slfRiskMapping/data/slfRisk/maxent_models/slftoh_downsampled_x10.tif")
toh <- raster("/Volumes/GoogleDrive/Shared drives/slfRiskMapping/data/slfRisk/maxent_models/toh_downsampled_x10.tif")


#fortify (dataframe-fy) the global model
slf_df <- fortify(slf, maxpixels = 1e10)
slftoh_df <- fortify(slftoh, maxpixels = 1e10)
toh_df <- fortify(toh, maxpixels = 1e10)

colnames(slf_df)[3] <- "value"
colnames(slftoh_df)[3] <- "value"
colnames(toh_df)[3] <- "value"

#remove the NA values to shrink the object size
slf_df <- slf_df[!is.na(slf_df$value),]
slftoh_df <- slftoh_df[!is.na(slftoh_df$value),]
toh_df <- toh_df[!is.na(toh_df$value),]

#round the value points
slf_df$value <- round(slf_df$value, digits = 2)
slftoh_df$value <- round(slftoh_df$value, digits = 2)
slf_df$value <- round(slf_df$value, digits = 2)

#add in the countries center for plotting the map ***TURNED OFF RIGHT NOW***
#read in the centroids for a check
#countries_centers <- read_csv(file = "./data-raw/geopolitical_centers_world.csv") %>%
#  dplyr::rename(y = y_coord, x = x_coord)
#colnames(countries_centers)[1] <- "geopol_unit"

#add in the countries capitals for plotting the map
#read in the basic world cities data from Pareto Software, LLC, the owner of Simplemaps.com, available at: https://simplemaps.com/data/world-cities
countries_centers <- read_csv(file = "./data-raw/worldcities.csv") %>%
  dplyr::select(country, iso2, iso3, city_ascii, lng, lat, capital) %>%
  dplyr::rename(geopol_unit = country, y = lat, x = lng, city_type = capital, capital = city_ascii) %>%
  dplyr::arrange(geopol_unit) %>%
  dplyr::filter(city_type == "primary")

#fix Ivory Coast to be same as other files
countries_centers$geopol_unit <- gsub(pattern = "Côte D???Ivoire", replacement = "Ivory Coast", x = countries_centers$geopol_unit)

#need to filter out cases where a country has more than one primary capital listed to be just one of the capitals
countries_centers <- countries_centers %>%
  filter(!(geopol_unit == "South Africa" & capital != "Cape Town")) %>%
  filter(!(geopol_unit == "Benin" & capital != "Porto-Novo")) %>%
  filter(!(geopol_unit == "Bolivia" & capital != "Sucre")) %>%
  filter(!(geopol_unit == "Burma" & capital != "Nay Pyi Taw")) %>%
  filter(!(geopol_unit == "Burundi" & capital != "Gitega")) %>%
  filter(!(geopol_unit == "Ivory Coast" & capital != "Yamoussoukro")) %>%
  filter(!(geopol_unit == "Netherlands" & capital != "Amsterdam")) %>%
  filter(!(geopol_unit == "Sri Lanka" & capital != "Colombo")) %>%
  filter(!(geopol_unit == "Swaziland" & capital != "Mbabane")) %>%
  filter(!(geopol_unit == "Tanzania" & capital != "Dodoma"))

#rm the city_type col, it is not useful here
countries_centers <- countries_centers %>%
  dplyr::select(-city_type)

##########################################################
#suitability models
##########################################################

#read in presence records
slf_points <- read_csv(file = "./data-raw/slf_presence_records.csv") %>%
  dplyr::rename(species = Species, x = Longitude, y = Latitude) %>%
  mutate(species = "Lycorma delicatula") %>%
  dplyr::select(species, x, y)
toh_points <- read_csv(file = "./data-raw/toh_presence_records.csv") %>%
  dplyr::rename(species = name, x = decimalLongitude, y = decimalLatitude) %>%
  dplyr::select(species, x, y)


####################################################################################################################
#Write out new RDA files
####################################################################################################################

#############################
#wine market risk
#############################
save(countries_market, file = file.path(here(), "data", "countries_market.rda")) #wine import and export data by country

#############################
#trade
#############################
save(states_trade_summary_slf, file = file.path(here(), "data", "states_trade_summary_slf.rda")) #present value trade with infected states
save(states_trade_summary_slf_future, file = file.path(here(), "data", "states_trade_summary_slf_future.rda")) #future scenario value trade with infected states
save(states_trade_mass_summary_slf, file = file.path(here(), "data", "states_trade_mass_summary_slf.rda")) #present mass trade with infected states
save(states_trade_mass_summary_slf_future, file = file.path(here(), "data", "states_trade_mass_summary_slf_future.rda")) #future scenario mass trade with infected states

save(countries_trade_summary_slf, file = file.path(here(), "data", "countries_trade_summary_slf.rda")) #present value trade with infected states
save(countries_trade_summary_slf_future, file = file.path(here(), "data", "countries_trade_summary_slf_future.rda")) #future scenario value trade with infected states
save(countries_trade_mass_summary_slf, file = file.path(here(), "data", "countries_trade_mass_summary_slf.rda")) #present mass trade with infected states
save(countries_trade_mass_summary_slf_future, file = file.path(here(), "data", "countries_trade_mass_summary_slf_future.rda")) #future scenario mass trade with infected states

#############################
#grapes
#############################
save(states_grapes, file = file.path(here(), "data", "states_grapes.rda"))
save(countries_grapes, file = file.path(here(), "data", "countries_grapes.rda"))

#############################
#wine
#############################
save(states_wine, file = file.path(here(), "data", "states_wine.rda"))
save(countries_wine, file = file.path(here(), "data", "countries_wine.rda"))

#############################
#wineries
#############################
save(wineries, file = file.path(here(), "data", "wineries.rda"))

#############################
#suitability models
#############################
save(states_extracts, file = file.path(here(), "data", "states_extracts.rda"))
save(countries_extracts, file = file.path(here(), "data", "countries_extracts.rda"))

#############################
#suitability models rasters
#############################
save(suitability_usa_df, file = file.path(here(), "data", "suitability_usa_df.rda"))
save(slf_usa_df, file = file.path(here(), "data", "slf_usa_df.rda"))
save(slftoh_usa_df, file = file.path(here(), "data", "slftoh_usa_df.rda"))
save(toh_usa_df, file = file.path(here(), "data", "toh_usa_df.rda"))
save(suitability_countries_df, file = file.path(here(), "data", "suitability_countries_df.rda"))
save(states_centers, file = file.path(here(), "data", "states_centers.rda"))
save(countries_centers, file = file.path(here(), "data", "countries_centers.rda"))
save(slf_df, file = file.path(here(), "data", "slf_df.rda"))
save(slftoh_df, file = file.path(here(), "data", "slftoh_df.rda"))
save(toh_df, file = file.path(here(), "data", "toh_df.rda"))

#############################
#presence records for models
#############################
save(slf_points, file = file.path(here(), "data", "slf_points.rda"))
save(toh_points, file = file.path(here(), "data", "toh_points.rda"))

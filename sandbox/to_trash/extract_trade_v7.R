#grab trade data for SLF by state and countries
#Authors: NAH

#load relevent libraries
library(tidyverse)

#set working dir
setwd("/Users/nicholashuron/Google Drive File Stream/My Drive/spotted_lanternfly_ieco_projects/data/trade/FINAL_DATA/")

#switch to turn on future trade
present_switch <- FALSE

##########################################################################################################################################
#INTERSTATE
##########################################################################################################################################

#read in trade file
#interstate

###############################################
#VALUE
states_value <- read_csv(file = "./states_value_14_june_2019.csv", col_names = T)

#filter to remove in state commerce (origin == destination)
states_value <- states_value %>%
  filter(!destination == origin)

#first all trade by state
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

#here is where the switch includes extra states
#extra states now: NC, NY, OH, WV
if(present_switch == FALSE){
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
  
#new version of summarizing that works for new states
  #add all infected state data added to new summary object
  trade_summary_slf <- left_join(trade_summary, trade_pa, by = "destination") %>%
    left_join(., trade_de, by = "destination") %>%
    left_join(., trade_md, by = "destination") %>%
    left_join(., trade_nc, by = "destination") %>%
    left_join(., trade_nj, by = "destination") %>%
    left_join(., trade_ny, by = "destination") %>%
    left_join(., trade_oh, by = "destination") %>%
    left_join(., trade_va, by = "destination") %>%
    left_join(., trade_wv, by = "destination")
  
  #add info for the total of all infected states
  trade_summary_slf <- trade_summary_slf %>%
    group_by(destination) %>%
    mutate(
      infected_total = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T),
      infected_average = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T)/54 #54 is 6 years * 9 total infected states, previous average removes NA's, forgetting them in the mean calc
    )
  
  #write out results
  write_csv(x = trade_summary_slf, path = "./extract_trade_value_states_future.csv", col_names = T)
  
  
}  else{
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
  
  #write out results
  write_csv(x = trade_summary_slf, path = "./extract_trade_value_states.csv", col_names = T)
  
}




#SUMMARIZE TRADE
trade_summary_slf[is.na(trade_summary_slf)] <- 0

#change the name of DC
trade_summary_slf$destination <- gsub(pattern = "Washington DC", replacement = "District of Columbia", x = trade_summary_slf$destination)

#code snippet to attach infection status and add state abbreviations
#state id's for adding to extracts
stateid <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "DC", "WV", "WI", "WY")
stateid <- as_tibble(stateid, ncol =1)
colnames(stateid) <- "ID"


#add col to identify infected states
if(present_switch == FALSE){
  trade_summary_slf <-  bind_cols(trade_summary_slf, stateid) %>%
    mutate(status = ifelse(ID %in% c("DE", "MD", "NC", "NJ", "NY", "OH", "PA", "VA", "WV"), "established", "not established")) %>%
    dplyr::select(destination, ID, status, everything())
  
  #tidying of data
  states_infect_trade_value <- trade_summary_slf %>%
    gather(-destination:-status, key = "infected_state", value = "trade") %>%
    as.data.frame(.)
  
  #total infected states trade, average/SE
  
  #interstate
  #value
  states_infect_trade_value_se <- states_infect_trade_value %>%
    #filter(!destination %in% c("Delaware", "Maryland", "Pennsylvania", "Virginia", "New Jersey")) %>%
    group_by(destination) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_") | str_detect(infected_state, "nc_")  | str_detect(infected_state, "ny_")  | str_detect(infected_state, "oh_")  | str_detect(infected_state, "wv_")) %>%
    filter(str_detect(infected_state, "201") & !str_detect(infected_state, "all")) %>%
    mutate(tempcol = str_split(infected_state, "_")) %>%
    rowwise() %>%
    mutate(source = unlist(tempcol)[1], year = unlist(tempcol)[2]) %>%
    select(-tempcol) %>%
    group_by(destination, year, status) %>%
    #filter out the intrastate trade for infected states to avoid extra low averages
    mutate(clear_intra = paste0(destination, source)) %>%
    filter(!clear_intra %in% c("Delawarede", "Marylandmd", "New Jerseynj", "Pennsylvaniapa", "Virginiava", "North Carolinanc", "New Yorkny", "Ohiooh", "West Virginiawv")) %>%
    #remove workaround infected intrastate column
    dplyr::select(-clear_intra) %>%
    summarize(avg_infected_trade = sum(trade)) %>%
    group_by(destination, status) %>%
    mutate(se = (sd(avg_infected_trade, na.rm = T) / sqrt(length(year)))) %>%
    summarise(avg_infected_trade = mean(avg_infected_trade, na.rm = T), se = mean(se))
  
  #write out the summary results 
  write_csv(x = states_infect_trade_value_se, path = "./summary_states_trade_value_future_v2.csv", col_names = T)
  
  } else{
  trade_summary_slf <-  bind_cols(trade_summary_slf, stateid) %>%
    mutate(status = ifelse(ID %in% c("DE", "MD", "NJ","PA", "VA"), "established", "not established")) %>%
    dplyr::select(destination, ID, status, everything())
  
  #tidying of data
  states_infect_trade_value <- trade_summary_slf %>%
    gather(-destination:-status, key = "infected_state", value = "trade") %>%
    as.data.frame(.)
  
  #total infected states trade, average/SE
  
  #interstate
  #value
  states_infect_trade_value_se <- states_infect_trade_value %>%
    #filter(!destination %in% c("Delaware", "Maryland", "Pennsylvania", "Virginia", "New Jersey")) %>%
    group_by(destination) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_")) %>%
    filter(str_detect(infected_state, "201") & !str_detect(infected_state, "all")) %>%
    mutate(tempcol = str_split(infected_state, "_")) %>%
    rowwise() %>%
    mutate(source = unlist(tempcol)[1], year = unlist(tempcol)[2]) %>%
    select(-tempcol) %>%
    group_by(destination, year, status) %>%
    #filter out the intrastate trade for infected states to avoid extra low averages
    mutate(clear_intra = paste0(destination, source)) %>%
    filter(!clear_intra %in% c("Delawarede", "Marylandmd", "New Jerseynj", "Pennsylvaniapa", "Virginiava")) %>%
    #remove workaround infected intrastate column
    dplyr::select(-clear_intra) %>%
    summarize(avg_infected_trade = sum(trade)) %>%
    group_by(destination, status) %>%
    mutate(se = (sd(avg_infected_trade, na.rm = T) / sqrt(length(year)))) %>%
    summarise(avg_infected_trade = mean(avg_infected_trade, na.rm = T), se = mean(se))
  
  #write out the summary results 
  write_csv(x = states_infect_trade_value_se, path = "./summary_states_trade_value_v2.csv", col_names = T)
  
}



###############################################
#MASS

states_mass <- read_csv(file = "./states_mass_14_june_2019.csv", col_names = T)

#filter to remove in state commerce (origin == destination)
states_mass <- states_mass %>%
  filter(!destination == origin)

#first all trade by state
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



#here is where the switch includes extra states
#extra states now: NC, NY, OH, WV
if(present_switch == FALSE){
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
  
  #new version of summarizing that works for new states
  #add all infected state data added to new summary object
  trade_mass_summary_slf <- left_join(trade_mass_summary, trade_mass_pa, by = "destination") %>%
    left_join(., trade_mass_de, by = "destination") %>%
    left_join(., trade_mass_md, by = "destination") %>%
    left_join(., trade_mass_nc, by = "destination") %>%
    left_join(., trade_mass_nj, by = "destination") %>%
    left_join(., trade_mass_ny, by = "destination") %>%
    left_join(., trade_mass_oh, by = "destination") %>%
    left_join(., trade_mass_va, by = "destination") %>%
    left_join(., trade_mass_wv, by = "destination")
  
  #add info for the total of all infected states
  trade_mass_summary_slf <- trade_mass_summary_slf %>%
    group_by(destination) %>%
    mutate(
      infected_total = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T),
      infected_average = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T)/54 #54 is 6 years * 9 total infected states, previous average removes NA's, forgetting them in the mean calc
    )
  
  #write out results
  write_csv(x = trade_summary_slf, path = "./extract_trade_mass_states_future.csv", col_names = T)
  
  
}  else{

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
  
  #write out results
  write_csv(x = trade_mass_summary_slf, path = "./extract_trade_mass_states.csv", col_names = T)
  
}



trade_mass_summary_slf[is.na(trade_mass_summary_slf)] <- 0

#change the name of DC
trade_mass_summary_slf$destination <- gsub(pattern = "Washington DC", replacement = "District of Columbia", x = trade_mass_summary_slf$destination)

#code snippet to attach infection status and add state abbreviations
#state id's for adding to extracts
stateid <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "DC", "WV", "WI", "WY")
stateid <- as_tibble(stateid, ncol =1)
colnames(stateid) <- "ID"





#add col to identify infected states
if(present_switch == FALSE){
  #add col to identify infected states
  trade_mass_summary_slf <-  bind_cols(trade_mass_summary_slf, stateid) %>%
    mutate(status = ifelse(ID %in% c("DE", "MD", "NC", "NJ", "NY", "OH", "PA", "VA", "WV"), "established", "not established")) %>%
    dplyr::select(destination, ID, status, everything())
  
  
  #tidying of data and dividing by 1000 to convert from kg to metric tons
  states_infect_trade_mass <- trade_mass_summary_slf %>%
    gather(-destination:-status, key = "infected_state", value = "mass") %>%
    mutate(mass = mass / 1000) %>%
    as.data.frame(.)
  
  #mass
  states_infect_trade_mass_se <- states_infect_trade_mass %>%
    #filter(!destination %in% c("Delaware", "Maryland", "Pennsylvania", "Virginia", "New Jersey")) %>%
    group_by(destination) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_") | str_detect(infected_state, "nc_")  | str_detect(infected_state, "ny_")  | str_detect(infected_state, "oh_")  | str_detect(infected_state, "wv_")) %>%
    filter(str_detect(infected_state, "201") & !str_detect(infected_state, "all")) %>%
    mutate(tempcol = str_split(infected_state, "_")) %>%
    rowwise() %>%
    mutate(source = unlist(tempcol)[1], year = unlist(tempcol)[2]) %>%
    select(-tempcol) %>%
    group_by(destination, year, status) %>%
    #filter out the intrastate trade for infected states to avoid extra low averages
    mutate(clear_intra = paste0(destination, source)) %>%
    filter(!clear_intra %in% c("Delawarede", "Marylandmd", "New Jerseynj", "Pennsylvaniapa", "Virginiava", "North Carolinanc", "New Yorkny", "Ohiooh", "West Virginiawv")) %>%
    #remove workaround infected intrastate column
    dplyr::select(-clear_intra) %>%
    summarize(avg_infected_mass = sum(mass)) %>%
    group_by(destination, status) %>%
    mutate(se = (sd(avg_infected_mass, na.rm = T) / sqrt(length(year)))) %>%
    summarise(avg_infected_mass = mean(avg_infected_mass, na.rm = T), se = mean(se))
  
  #write out the summary results 
  write_csv(x = states_infect_trade_mass_se, path = "./summary_states_trade_mass_future_v2.csv", col_names = T)
  
} else{
  #add col to identify infected states
  trade_mass_summary_slf <-  bind_cols(trade_mass_summary_slf, stateid) %>%
    mutate(status = ifelse(ID %in% c("DE", "MD", "NJ","PA", "VA"), "established", "not established")) %>%
    dplyr::select(destination, ID, status, everything())
  
  
  #tidying of data and dividing by 1000 to convert from kg to metric tons
  states_infect_trade_mass <- trade_mass_summary_slf %>%
    gather(-destination:-status, key = "infected_state", value = "mass") %>%
    mutate(mass = mass / 1000) %>%
    as.data.frame(.)
  
  #mass
  states_infect_trade_mass_se <- states_infect_trade_mass %>%
    #filter(!destination %in% c("Delaware", "Maryland", "Pennsylvania", "Virginia", "New Jersey")) %>%
    group_by(destination) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_")) %>%
    filter(str_detect(infected_state, "201") & !str_detect(infected_state, "all")) %>%
    mutate(tempcol = str_split(infected_state, "_")) %>%
    rowwise() %>%
    mutate(source = unlist(tempcol)[1], year = unlist(tempcol)[2]) %>%
    select(-tempcol) %>%
    group_by(destination, year, status) %>%
    #filter out the intrastate trade for infected states to avoid extra low averages
    mutate(clear_intra = paste0(destination, source)) %>%
    filter(!clear_intra %in% c("Delawarede", "Marylandmd", "New Jerseynj", "Pennsylvaniapa", "Virginiava")) %>%
    #remove workaround infected intrastate column
    dplyr::select(-clear_intra) %>%
    summarize(avg_infected_mass = sum(mass)) %>%
    group_by(destination, status) %>%
    mutate(se = (sd(avg_infected_mass, na.rm = T) / sqrt(length(year)))) %>%
    summarise(avg_infected_mass = mean(avg_infected_mass, na.rm = T), se = mean(se))
  
  #write out the summary results 
  write_csv(x = states_infect_trade_mass_se, path = "./summary_states_trade_mass_v2.csv", col_names = T)
  
}







##########################################################################################################################################
#International
##########################################################################################################################################

#VALUE
countries_value <- read_csv(file = "./countries_value_14_june_2019.csv", col_names = T)


#first all trade by state
#this gets totals and averages per destination across years and for individual years
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

#here is where the switch includes extra states
#extra states now: NC, NY, OH, WV
if(present_switch == FALSE){
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
  #new version of summarizing that works for new states
  #add all infected state data added to new summary object
  trade_summary_slf <- left_join(trade_summary, trade_pa, by = "destination") %>%
    left_join(., trade_de, by = "destination") %>%
    left_join(., trade_md, by = "destination") %>%
    left_join(., trade_nc, by = "destination") %>%
    left_join(., trade_nj, by = "destination") %>%
    left_join(., trade_ny, by = "destination") %>%
    left_join(., trade_oh, by = "destination") %>%
    left_join(., trade_va, by = "destination") %>%
    left_join(., trade_wv, by = "destination")
  
  #add info for the total of all infected states
  trade_summary_slf <- trade_summary_slf %>%
    group_by(destination) %>%
    mutate(
      infected_total = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T),
      infected_average = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T)/54 #54 is 6 years * 9 total infected states, previous average removes NA's, forgetting them in the mean calc
    )
  
  #write out results
  write_csv(x = trade_summary_slf, path = "./extract_trade_value_countries_future.csv", col_names = T)
  
  
}  else{
  
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

#write out results
write_csv(x = trade_summary_slf, path = "./extract_trade_value_countries.csv", col_names = T)

}

trade_summary_slf[is.na(trade_summary_slf)] <- 0


#tidying of data and log transforming values
countries_infect_trade_value <- trade_summary_slf %>%
  gather(-destination, key = "infected_state", value = "trade") %>%
  as.data.frame(.)


if(present_switch == FALSE){
  #get top 50 countries for trade value
  top50_countries <- countries_infect_trade_value %>%
    filter(str_detect(infected_state, "average")) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_") | str_detect(infected_state, "nc_")  | str_detect(infected_state, "ny_")  | str_detect(infected_state, "oh_")  | str_detect(infected_state, "wv_")) %>%
    group_by(destination) %>%
    summarize(total = sum(trade)) %>%
    arrange(desc(total)) %>%
    .[["destination"]] %>%
    .[1:50]
  
  #value
  countries_infect_trade_value_se <- countries_infect_trade_value %>%
    group_by(destination) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_") | str_detect(infected_state, "nc_")  | str_detect(infected_state, "ny_")  | str_detect(infected_state, "oh_")  | str_detect(infected_state, "wv_")) %>%
    filter(str_detect(infected_state, "201") & !str_detect(infected_state, "all")) %>%
    mutate(tempcol = str_split(infected_state, "_")) %>%
    rowwise() %>%
    mutate(source = unlist(tempcol)[1], year = unlist(tempcol)[2]) %>%
    select(-tempcol) %>%
    group_by(destination, year) %>%
    summarize(avg_infected_trade = sum(trade)) %>%
    group_by(destination) %>%
    mutate(se = (sd(avg_infected_trade, na.rm = T) / sqrt(length(year)))) %>%
    summarise(avg_infected_trade = mean(avg_infected_trade, na.rm = T), se = mean(se))
  
  #write out the summary results 
  write_csv(x = countries_infect_trade_value_se, path = "./summary_countries_trade_value_future.csv", col_names = T)
  
  
} else{
  
  #get top 50 countries for trade value
  top50_countries <- countries_infect_trade_value %>%
    filter(str_detect(infected_state, "average")) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_")| str_detect(infected_state, "va_")| str_detect(infected_state, "nj_")| str_detect(infected_state, "md_")) %>%
    group_by(destination) %>%
    summarize(total = sum(trade)) %>%
    arrange(desc(total)) %>%
    .[["destination"]] %>%
    .[1:50]
 
  #value
  countries_infect_trade_value_se <- countries_infect_trade_value %>%
    group_by(destination) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_")) %>%
    filter(str_detect(infected_state, "201") & !str_detect(infected_state, "all")) %>%
    mutate(tempcol = str_split(infected_state, "_")) %>%
    rowwise() %>%
    mutate(source = unlist(tempcol)[1], year = unlist(tempcol)[2]) %>%
    select(-tempcol) %>%
    group_by(destination, year) %>%
    summarize(avg_infected_trade = sum(trade)) %>%
    group_by(destination) %>%
    mutate(se = (sd(avg_infected_trade, na.rm = T) / sqrt(length(year)))) %>%
    summarise(avg_infected_trade = mean(avg_infected_trade, na.rm = T), se = mean(se))
  
  #write out the summary results 
  write_csv(x = countries_infect_trade_value_se, path = "./summary_countries_trade_value.csv", col_names = T)
   
}




###############################################
#MASS

countries_mass <- read_csv(file = "./countries_mass_14_june_2019.csv", col_names = T)

#first all trade by state
#this gets totals and averages per destination across years and for individual years
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

#here is where the switch includes extra states
#extra states now: NC, NY, OH, WV
if(present_switch == FALSE){
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
  
  #new version of summarizing that works for new states
  #add all infected state data added to new summary object
  trade_mass_summary_slf <- left_join(trade_mass_summary, trade_mass_pa, by = "destination") %>%
    left_join(., trade_mass_de, by = "destination") %>%
    left_join(., trade_mass_md, by = "destination") %>%
    left_join(., trade_mass_nc, by = "destination") %>%
    left_join(., trade_mass_nj, by = "destination") %>%
    left_join(., trade_mass_ny, by = "destination") %>%
    left_join(., trade_mass_oh, by = "destination") %>%
    left_join(., trade_mass_va, by = "destination") %>%
    left_join(., trade_mass_wv, by = "destination")
  
  #add info for the total of all infected states
  trade_mass_summary_slf <- trade_mass_summary_slf %>%
    group_by(destination) %>%
    mutate(
      infected_total = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T),
      infected_average = sum(c(de_total, md_total, nc_total, nj_total, ny_total, oh_total, pa_total, va_total, wv_total), na.rm = T)/54 #54 is 6 years * 9 total infected states, previous average removes NA's, forgetting them in the mean calc
    )
  
  #write out results
  write_csv(x = trade_mass_summary_slf, path = "./extract_trade_mass_countries_future.csv", col_names = T)
  
}  else{

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

#write out results
write_csv(x = trade_mass_summary_slf, path = "./extract_trade_mass_countries.csv", col_names = T)

}

trade_mass_summary_slf[is.na(trade_mass_summary_slf)] <- 0

#tidying of data and converting from kg to metric tons 
countries_infect_trade_mass <- trade_mass_summary_slf %>%
  gather(-destination, key = "infected_state", value = "mass") %>%
  mutate(mass = mass / 1000) %>%
  as.data.frame(.)


if(present_switch == FALSE){
  #get top 50 countries for trade value
  top50_countries <- countries_infect_trade_mass %>%
    filter(str_detect(infected_state, "average")) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_") | str_detect(infected_state, "nc_")  | str_detect(infected_state, "ny_")  | str_detect(infected_state, "oh_")  | str_detect(infected_state, "wv_")) %>%
    group_by(destination) %>%
    summarize(total = sum(mass)) %>%
    arrange(desc(total)) %>%
    .[["destination"]] %>%
    .[1:50]
  
  #mass
  countries_infect_trade_mass_se <- countries_infect_trade_mass %>%
    group_by(destination) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_") | str_detect(infected_state, "nc_")  | str_detect(infected_state, "ny_")  | str_detect(infected_state, "oh_")  | str_detect(infected_state, "wv_")) %>%
    filter(str_detect(infected_state, "201") & !str_detect(infected_state, "all")) %>%
    mutate(tempcol = str_split(infected_state, "_")) %>%
    rowwise() %>%
    mutate(source = unlist(tempcol)[1], year = unlist(tempcol)[2]) %>%
    select(-tempcol) %>%
    group_by(destination, year) %>%
    summarize(avg_infected_mass = sum(mass)) %>%
    group_by(destination) %>%
    mutate(se = (sd(avg_infected_mass, na.rm = T) / sqrt(length(year)))) %>%
    summarise(avg_infected_mass = mean(avg_infected_mass, na.rm = T), se = mean(se))
  
  #write out the summary results 
  write_csv(x = countries_infect_trade_mass_se, path = "./summary_countries_trade_mass_future.csv", col_names = T)
  

} else{
  #get top 50 countries for trade value
  top50_countries <- countries_infect_trade_mass %>%
    filter(str_detect(infected_state, "average")) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_")| str_detect(infected_state, "va_")| str_detect(infected_state, "nj_")| str_detect(infected_state, "md_")) %>%
    group_by(destination) %>%
    summarize(total = sum(mass)) %>%
    arrange(desc(total)) %>%
    .[["destination"]] %>%
    .[1:50]
  
  #mass
  countries_infect_trade_mass_se <- countries_infect_trade_mass %>%
    group_by(destination) %>%
    filter(str_detect(infected_state, "pa_") | str_detect(infected_state, "de_") | str_detect(infected_state, "va_") | str_detect(infected_state, "nj_") | str_detect(infected_state, "md_")) %>%
    filter(str_detect(infected_state, "201") & !str_detect(infected_state, "all")) %>%
    mutate(tempcol = str_split(infected_state, "_")) %>%
    rowwise() %>%
    mutate(source = unlist(tempcol)[1], year = unlist(tempcol)[2]) %>%
    select(-tempcol) %>%
    group_by(destination, year) %>%
    summarize(avg_infected_mass = sum(mass)) %>%
    group_by(destination) %>%
    mutate(se = (sd(avg_infected_mass, na.rm = T) / sqrt(length(year)))) %>%
    summarise(avg_infected_mass = mean(avg_infected_mass, na.rm = T), se = mean(se))
  
  #write out the summary results 
  write_csv(x = countries_infect_trade_mass_se, path = "./summary_countries_trade_mass.csv", col_names = T)
  
}


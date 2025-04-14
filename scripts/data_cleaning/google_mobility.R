##### Clean Google COVID-19 Community Mobility Reports Data ####
#### Calculate mobility = mean of mobility in non-residential spaces 
### (retail & recreation, grocery & pharmacy, parks, transit, worplaces)
## See: 
# https://www.google.com/covid19/mobility/
# https://www.google.com/covid19/mobility/data_documentation.html?hl=en

#### Setup ####
rm(list = ls())
# Load packages
source("scripts/utilities/load_packages.R")

## Load data
Google <- read_csv("data/raw_data/Global_Mobility_Report.csv")

### Clean data ####
#### Subset data that is only on the national level
Google_national <- filter(Google, is.na(sub_region_1) & is.na(metro_area)) %>% 
  # Get rid of subnational markers & other extraneous variables
  select(country_region_code, country_region, date:workplaces_percent_change_from_baseline)

#### Calculate mean non-residential mobility ####  
#### Calculate daily mean of non-residential mobility
Google_mean <- group_by(Google_national, country_region_code, country_region, date) %>% 
  summarise(mean_mobility = mean(c_across(retail_and_recreation_percent_change_from_baseline:workplaces_percent_change_from_baseline)))

### Combine back into main data
Google_clean <- full_join(Google_national, Google_mean)

#### Export data ####
setwd("data/clean_data")
write_csv(Google_clean, "Google_mobility.csv")

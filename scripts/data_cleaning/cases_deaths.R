#### Clean epidemiological data (cases & deaths) ####
#### Obtain: Official deaths & cases (per 100,000) 
### Source- Our World in Data: https://github.com/owid/covid-19-data/tree/master/public/data/
## Wanted to include hospitalizations, but too few countries actually report the data

#### Setup ####
rm(list = ls())
# Load packages
source("scripts/utilities/load_packages.R")

# Load data (from Our World in Data repo)
Owid <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>% 
  read_csv()

### Clean data ####
#### Select relevant varibles
Owid_cut <- select(Owid, iso_code, location, date, 
                   new_cases_per_million, new_deaths_per_million)

#### Convert per million to per 100,000
Owid_clean <- mutate(Owid_cut, cases_per_100k = new_cases_per_million*10, 
                     deaths_per_100k = new_deaths_per_million*10) %>% 
  select(-c(new_cases_per_million, new_deaths_per_million))

#### Export data ####
setwd("data/clean_data")
write_csv(Owid_clean, "Cases_deaths_clean.csv")

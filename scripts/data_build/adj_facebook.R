#### Adjust Facebook data for selection bias ####
#### Get raw expats data by age, gender, origin, destination, date
## Have ground truth for all foreign born 
## Also- cells are too small to do this (even if use largest age groupings & don't break down genders, median still 1000)

#### Setup ####
rm(list = ls())
setwd(here::here())
# Load packages
source("scripts/utilities/load_packages.R")

# List of countries included in analysis
clist <- c("US","CA","GB","ES","FR","IT","BE","NL","DE","TR","AE","EG","MA","DZ","TN","MR","ML","BF","SN","GM","GN","CI","BJ","GA")

#### Get raw facebook expats by age, gender, origin, destination, date ####
#### Load
setwd("data/clean_data")
FB <- read_csv("facebook/FB_full_processed.csv")

#### Clean
# Filter out all ages + aggregated ages
FB_expats <- filter(FB, age != "13-" & age != "15-24" & age != "25-64") %>% 
  # Gender
  filter(genders != "both") %>%
  # Country of origin
  filter(grepl("Algeria|Morocco|Senegal|Ivoire", citizenship)) %>% 
  mutate(origin = case_when(grepl("Algeria", citizenship) ~ "DZ", 
                            grepl("Morocco", citizenship) ~ "MA", 
                            grepl("Senegal", citizenship) ~ "SN", 
                            grepl("Ivoire", citizenship) ~ "CI")) %>% 
  # Cut SN from destination countries
  filter(country != "SN") %>% 
  # Organize & drop unneeded variables
  select(mau_audience, age, genders, origin, country, date)

#### Official statistics ####
#### USA- ACS
### Load
# Not in project directory, file too large
setwd("~/Documents/Documents - opr-jdklein-mac/Projects/MPIDR/Migrant stock")
ACS <- gzfile("ACS.csv.gz") %>% 
  read_csv

### Clean
## Cut to make manageable size
# Get 2019 data
#*Not using 2020 data, census doesn't trust it because of covid
ACS_cut <- filter(ACS, YEAR == 2019) %>% 
  # Only select origin countries of interest 
  # (Algeria = 60011, Morocco = 60014, Cote d'Ivoire = 60026, Senegal = 60032)
  filter(grepl("60011|60014|60026|60032", BPLD)) %>% 
  # Cut out children < 13
  filter(AGE >= 13)

## Get grouped into categories used in facebook data
# Get age groups
ACS_grouped <- unique(FB_expats$age) %>% 
  strtrim(2) %>% 
  c(as.numeric(), 120) %>% 
  cut(ACS_cut$AGE, ., right = F, unique(FB_expats$age)) %>% 
  mutate(ACS_cut, age = .) %>% 
  # Get gender (ACS uses sex, facebook uses gender)
  mutate(genders = case_when(SEX == 1 ~ "male", 
                             SEX == 2 ~ "female")) %>% 
  # Get origin countries from place of birth
  mutate(origin = case_when(BPLD == 60011 ~ "DZ", BPLD == 60014 ~ "MA", 
                            BPLD == 60026 ~ "CI", BPLD == 60032 ~ "SN")) %>% 
  # Country of destination = USA for all
  mutate(country = "US") %>% 
  # Sum of pop in each group 
  group_by(age, genders, origin, country, YEAR) %>% 
  summarise(population = sum(PERWT), .groups = "keep") %>% 
  ungroup() %>% 
  tidyr::complete(age, genders, origin, country, YEAR) %>% 
  mutate(population = if_else(is.na(population), 0, population))
# Since 2020 data unreliable, assume 2019 ~ 2020
ACS_grouped <- ACS_grouped %>% 
  mutate(YEAR = 2020) %>% 
  bind_rows(ACS_grouped)

### Question? - 13-14 has empty cells, should I group in w/ 15-19

#### Europe (Eurostat) ####
EU <- gzfile("migr_pop3ctb_linear.csv.gz") %>% 
  read_csv()

#### Build full dataset frorm clean data ####
#### Outcome (Facebook expats per 100,000 migrants)
#### Treatment (Travel Restrictions)
#### Covariates

#### Setup ####
rm(list = ls())
setwd(here::here())
# Load packages
source("scripts/utilities/load_packages.R")

# List of countries included in analysis
clist <- c("US","CA","GB","ES","FR","IT","BE","NL","DE","TR","AE","EG","MA","DZ","TN","MR","ML","BF","SN","GM","GN","CI","BJ","GA")

#### Facebook data (outcome, expats per 100,000) ####
#### Load
setwd("data/clean_data")
FB <- read_csv("facebook/FB_expats_per100k_unadj.csv")

#### Clean
## Order 
FB <- FB[order(FB$origin, FB$country, FB$date), ]
## Drop unused variables
FB_clean <- FB %>% 
  select(country, origin, date, expats_per_100k) %>% 
  # Cut SN from destination countries
  filter(country != "SN") %>% 
  # add origin-destination pairs indicex
  mutate(o_d_pair = paste0(origin, "-", country))
## Numerical origin-destination pair IDs
FB_clean <- tibble(o_d_pair = unique(FB_clean$o_d_pair), od_id = as.numeric(seq(1:length(unique(FB_clean$o_d_pair))))) %>% 
  full_join(FB_clean, .)

#### Create date key- duration & cutoffs of time intervals in the facebook data
# Time interval cutoff dates
cutoff_dates <- c(min(FB$date)-1, unique(FB$date), ymd("2020-06-01"))
# Date key for full facebook data
date_key_full <- tibble(duration = (cutoff_dates-lag(cutoff_dates))[-1], 
                   date = cutoff_dates[-1], time_interval = c(1:length(cutoff_dates[-1])))
# Date key for when travel restrictions in effect
date_key_short <- filter(date_key_full, time_interval >= 10)

#### Add date key back to Facebook
FB_clean <- select(FB_clean, origin, country, o_d_pair, od_id, date, expats_per_100k) %>%
  left_join(date_key_full) %>% 
    select(origin:date, time_interval, duration, expats_per_100k)

##### Combine Data w/ Migrant stock data to calculate log total expats
## Migrant stock official data
USEU_stock <- read_csv("EU_US_migrant_stock.csv")
UN_stock <- read_csv("UN_migrant_stock.csv")

### Expats from o in country d
### Clean migrant stock data- use US & EU data instead of UN data for those countries
Stock <- filter(UN_stock, !(country_code %in% unique(USEU_stock$country_code))) %>% 
  select(-country_name) %>% 
  bind_rows(USEU_stock)

### Combine back w/ facebook data
FB_clean <- mutate(FB_clean, Year = year(date)) %>%
  left_join(Stock, by = c("country" = "country_code", "Year")) %>% 
  mutate(log_expats = log(expats_per_100k/100000*population))

#### Add treatment to data (travel restrictions) ####
#### Load
Travel <- read_csv("travel_restriction_matrix_updated.csv")

#### Clean
## Convert date to date format & travel restrictions to factor
Travel_clean <- mutate(Travel, date = mdy(date), status = as.factor(status)) %>% 
  rename(country = destination)

## Fill in dates between 2020-03-08 back to 2020-02-16 
# This was the first month restrictions were in place, we measure the treatment over the whole month & assume
# if no restrictions for 1st date available (2020-03-08) -> no restrictions back to 2020-02-16
Travel_clean <- full_join(Travel_clean, 
                          expand_grid(unique(Travel_clean[, 1:2]), 
                                      date = seq.Date(min(date_key_short$date), max(date_key_short$date), "day"))) %>% 
  mutate(status = if_else(is.na(status), status[1], status)) %>% 
  # Combine w date key
  full_join(date_key_short)

## Order & fill in blanks
# Order
Travel_clean <- Travel_clean[order(Travel_clean$origin, Travel_clean$country, Travel_clean$date), ] 
# Fill in duration blanks
Travel_clean <- setDT(Travel_clean)[, duration := na.locf(duration, na.rm = F, fromLast = T), origin:country]
# Fill in time interval blanks
Travel_clean <- setDT(Travel_clean)[, time_interval := na.locf(time_interval, na.rm = F, fromLast = T), origin:country]

#### Calculate restriction indices
Travel_calculated <- Travel_clean %>% 
  group_by(origin, country, time_interval) %>% 
  count(status, .drop = F) %>% 
  pivot_wider(names_from = status, values_from = n) %>% 
  mutate(travel_rest_ind = red+.5*yellow) %>% 
  full_join(Travel_clean, .) %>% 
  mutate(travel_rest_ind = travel_rest_ind/as.numeric(duration))

#### Combine Facebook & travel restriction data
Combined <- select(Travel_calculated, origin:date, duration, time_interval, travel_rest_ind) %>%
  left_join(FB_clean, .) 

#### Add covariates to data ####
##### Covariates = pre/post pandemic dummy
#### Excess mortality (origin & destination, no lag & lag-1)
#### Stringency (origin & destination), Mobility (origin & destination)
#### Cases (origin & destination), official mortality (origin & destination, no lag & lag-1)
#### Visa requirements (from origin in destination) ???

#### Dummy for pre-post pandemic start (W) ####
Combined <- mutate(Combined, W = if_else(date >= ymd("2020-03-18"), 1, 0))

#### Excess mortality ####
#### Load
ExM <- read_csv("exmort.csv")

#### Clean
## Create excess mortality best estimate (use owid, if dont have use economist)
ExM_clean <- ExM %>% 
  mutate(excess_per_100k_daily_best = case_when(!is.na(excess_per_100k_daily_owid) ~ excess_per_100k_daily_owid, 
                                                is.na(excess_per_100k_daily_owid) ~ excess_per_100k_daily_economist))
# Fill in dates back to start of FB data (2019-04-17) & combine w date key
ExM_clean <- full_join(ExM_clean, expand_grid(country = unique(ExM$country), 
                                              date = seq.Date(min(date_key_full$date), max(date_key_full$date), "day"))) %>% 
  full_join(date_key_full)
# Order & fill in blanks
ExM_clean <- ExM_clean[order(ExM_clean$country, ExM_clean$date), ] 
ExM_clean <- setDT(ExM_clean)[, duration := na.locf(duration, na.rm = F, fromLast = T), country]
ExM_clean <- setDT(ExM_clean)[, time_interval := na.locf(time_interval, na.rm = F, fromLast = T), country]

#### Calculate excess mortality w/in each time interval (sum)
ExM_calculated <- ExM_clean %>% 
  group_by(country, time_interval) %>% 
  summarise(excess_mortality_per_100k = sum(excess_per_100k_daily_best)) %>% 
  full_join(ExM_clean, .) %>% 
  # Select only variables needed
  select(country, date, time_interval, duration, excess_mortality_per_100k)

#### Combine data- excess mortality in origin & destination
Combined <- rename(ExM_calculated, origin = country, excess_mortality_per_100k.o = excess_mortality_per_100k) %>% 
  left_join(Combined, .) %>% 
  left_join(., ExM_calculated) %>% 
  rename(excess_mortality_per_100k.d = excess_mortality_per_100k)
## Add lag-1
Combined <- group_by(Combined, o_d_pair) %>% 
  mutate(L1.excess_mortality_per_100k.o = lag(excess_mortality_per_100k.o), 
         L1.excess_mortality_per_100k.d = lag(excess_mortality_per_100k.d)) %>% 
  ungroup()
  
#### Oxford COVID-19 Government Response Tracker stringency index ####
### (without international travel restrictions)
### Load
OxCGRT <- read_csv("OxCGRT_clean.csv")

### Clean
String_clean <-  OxCGRT %>% 
  # Select countries of interest
  mutate(country = countrycode(CountryCode, "iso3c", "iso2c")) %>% 
  filter(grepl(paste0(clist, collapse = "|"), country)) %>% 
  # Drop unused variables & rename others
  select(country, date = Date, stringency_index = Stringency_new) %>% 
  # Select dates before 2020-06-01
  filter(date <= ymd("2020-06-01"))
# Fill in dates back to start of FB data (2019-04-17) & combine w date key
String_clean <- full_join(String_clean, expand_grid(country = unique(String_clean$country), 
                                                    date = seq.Date(min(date_key_full$date), max(date_key_full$date), "day"))) %>% 
  full_join(date_key_full)
# Order & fill in blanks
String_clean <- String_clean[order(String_clean$country, String_clean$date), ] 
String_clean <- setDT(String_clean)[, duration := na.locf(duration, na.rm = F, fromLast = T), country]
String_clean <- setDT(String_clean)[, time_interval := na.locf(time_interval, na.rm = F, fromLast = T), country]
# Insert 0s to fill out time interval 10 (Jan 19th & 20th 2020)
String_clean <- String_clean %>% 
  mutate(stringency_index = if_else(time_interval == 10 & is.na(stringency_index), 0, stringency_index))

#### Calculate w/in interval stringency (mean)
String_calculated <- String_clean %>% 
  group_by(country, time_interval) %>% 
  summarise(string_mean = mean(stringency_index)) %>% 
  full_join(String_clean, .) %>%
  # Select only variables needed
  select(-stringency_index)

#### Combine data- stringency index in origin & destination
Combined <- rename(String_calculated, origin = country, string_mean.o = string_mean) %>% 
  left_join(Combined, .) %>% 
  left_join(., String_calculated) %>% 
  rename(string_mean.d = string_mean)

#### Google mobility ####
#### Load
Google <- read_csv("Google_mobility.csv")

#### Clean
Google_clean <- Google %>% 
  # Select countries of interest
  filter(grepl(paste0(clist, collapse = "|"), country_region_code)) %>% 
  # Drop unused variables & rename others
  select(country = country_region_code, date, mean_mobility) %>% 
  # Select dates before 2020-06-01
  filter(date <= ymd("2020-06-01"))

# Fill in dates back to start of FB data (2019-04-17) & combine w date key
Google_clean <- full_join(Google_clean, expand_grid(country = unique(Google_clean$country), 
                                                    date = seq.Date(min(date_key_full$date), max(date_key_full$date), "day"))) %>% 
  full_join(date_key_full)
# Order & fill in blanks
Google_clean <- Google_clean[order(Google_clean$country, Google_clean$date), ] 
Google_clean <- setDT(Google_clean)[, duration := na.locf(duration, na.rm = F, fromLast = T), country]
Google_clean <- setDT(Google_clean)[, time_interval := na.locf(time_interval, na.rm = F, fromLast = T), country]
# Insert 0s for Jan 3-Feb 6th (this is the baseline against which everything else is a percentage change)
Google_clean <- Google_clean %>% 
  mutate(mean_mobility = if_else(date >= ymd("2020-01-03") & date <= ymd("2020-02-06") & is.na(mean_mobility), 0, mean_mobility))

#### Calculate w/in interval mobility (mean)
Google_calculated <- Google_clean %>% 
  group_by(country, time_interval) %>% 
  summarise(mobility_mean = mean(mean_mobility, na.rm = T))

#### Combine data- mobility in origin & destination
Combined <- rename(Google_calculated, origin = country, mobility_mean.o = mobility_mean) %>% 
  left_join(Combined, .) %>% 
  left_join(., Google_calculated) %>% 
  rename(mobility_mean.d = mobility_mean)
  
#### Cases & deaths ####
#### Cases (origin & destination), official mortality (origin & destination, no lag & lag-1)
#### Load
Cases_deaths <- read_csv("Cases_deaths_clean.csv")

#### Clean
Cases_deaths_clean <- Cases_deaths %>% 
  # Select countries of interest
  mutate(country = countrycode(iso_code, "iso3c", "iso2c")) %>% 
  filter(grepl(paste0(clist, collapse = "|"), country)) %>% 
  # Drop unused variables & rename others
  select(country, date, cases_per_100k, deaths_per_100k) %>% 
  # Select dates before 2020-06-01
  filter(date <= ymd("2020-06-01"))

## Fill in dates back to 2020-02-16 for all countries & combine w/ date key
# Not every country has case & death data that goes back to when travel restrictions were first put in place
Cases_deaths_clean <- full_join(Cases_deaths_clean, 
                          expand_grid(country = unique(Cases_deaths_clean$country), 
                                      date = seq.Date(min(date_key_short$date), max(date_key_short$date), "day"))) %>% 
  full_join(date_key_short)
# Order & fill in blanks
Cases_deaths_clean <- Cases_deaths_clean[order(Cases_deaths_clean$country, Cases_deaths_clean$date), ] 
Cases_deaths_clean <- setDT(Cases_deaths_clean)[, duration := na.locf(duration, na.rm = F, fromLast = T), country]
Cases_deaths_clean <- setDT(Cases_deaths_clean)[, time_interval := na.locf(time_interval, na.rm = F, fromLast = T), country]

#### Calculate w/in interval cases & deaths (sum)
## Since this is based on officially reported cases & deaths, we consider none reported = 0
Cases_deaths_calculated <- Cases_deaths_clean %>% 
  group_by(country, time_interval) %>% 
  summarise(cases_per_100k = sum(cases_per_100k, na.rm = T), deaths_per_100k = sum(deaths_per_100k, na.rm = T))

#### Combine data- cases & deaths in origin & destination
## Origin
Combined <- rename(Cases_deaths_calculated, origin = country, cases_per_100k.o = cases_per_100k, deaths_per_100k.o = deaths_per_100k) %>% 
  left_join(Combined, .) %>% 
  ## Destination
  left_join(., Cases_deaths_calculated) %>% 
  rename(cases_per_100k.d = cases_per_100k, deaths_per_100k.d = deaths_per_100k)

## Add deaths lag 1 (origin & destination)
Combined <- group_by(Combined, o_d_pair) %>% 
  mutate(L1.deaths_per_100k.o = lag(deaths_per_100k.o), 
         L1.deaths_per_100k.d = lag(deaths_per_100k.d)) %>% 
  ungroup() %>% 
  # drop year & total expat population from final data
  select(-Year, -population)

#### Export combined full dataset ####
# unadjusted for penetration
write_csv(Combined, "full_unadj.csv")

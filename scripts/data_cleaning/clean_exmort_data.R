#### Excess mortality data ####
#### Packages
library(tidyverse)
library(lubridate)
library(zoo)
library(RCurl)
library(countrycode)
library(data.table)

#### Read data ####
#### OWID github
### OWID excess mortality (HMD & WMD)
owid <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/excess_mortality.csv") %>% 
  read_csv()
### Economist 
economist <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/excess_mortality_economist_estimates.csv") %>% 
  read_csv()

## Get list of countries
clist <- c("US","CA","GB","ES","FR","IT","BE","NL","DE","TR","AE","EG","MA","DZ","TN","MR","ML","BF","SN","GM","GW","GN","CI","BJ","GA")

#### Work w data ####
### OWID
### Select countries & time (before 6/1) I need need
owid_cut <- mutate(owid, country = countrycode(location, "country.name", "iso2c")) %>% 
  filter(grepl(paste0(clist, collapse = "|"), country)) %>% 
  filter(date <= ymd("2020-06-01"))

#### Select variables I need (excess per million)
owid_clean <- select(owid_cut, country, date, time, time_unit, excess_per_million_proj_all_ages)

##### Economist
### Select countries & time (before 6/1) I need
econ_cut <- mutate(economist, country = countrycode(country, "country.name", "iso2c")) %>% 
  filter(grepl(paste0(clist, collapse = "|"), country)) %>% 
  filter(date <= ymd("2020-06-01"))

### Select variabnles I need (estimated_daily_excess_deaths_per_100k)
econ_clean <- select(econ_cut, country, date, estimated_daily_excess_deaths_per_100k)
## Extend dates & fill
econ_clean <- econ_clean %>% 
  full_join(expand_grid(country = clist, date = seq.Date(ymd("2020-01-01"), ymd("2020-06-01"), "day")))
econ_clean <- econ_clean[order(econ_clean$country, econ_clean$date), ] %>% 
  mutate(estimated_daily_excess_deaths_per_100k = na.locf(estimated_daily_excess_deaths_per_100k))

#### Combine data
### Pivot owid wider (weekly & monthly estimates)
owid_wide <- pivot_wider(owid_clean, names_from = time_unit, values_from = excess_per_million_proj_all_ages, 
              names_prefix = "excess_per_million_")
### Join
Exmort_full <- full_join(econ_clean, owid_wide)

### Clean- separate vars for economist & owid estimates
names(Exmort_full)[3] <- c("excess_per_100k_daily_economist")

### Create daily 100k estimates from OWID data
# Weekly fill
Exmort_full <- setDT(Exmort_full)[, excess_per_million_weekly_filled := 
                     na.locf(excess_per_million_weekly, na.rm = F, fromLast = T), country]
# Monthly fill
Exmort_full <- setDT(Exmort_full)[, excess_per_million_monthly_filled := 
                                    na.locf(excess_per_million_monthly, na.rm = F, fromLast = T), country]
# Get time index & duration of each interval
Exmort_full <- setDT(Exmort_full)[, time := na.locf(time, na.rm = F, fromLast = T), country]
Exmort_full <- Exmort_full %>%
  group_by(country, time, .drop = T) %>% 
  summarise(time_len = n()) %>% 
  full_join(Exmort_full)

### Calculation- (weeks/(7*10), months/(len*10))
Exmort_full <- Exmort_full %>% 
  mutate(excess_per_100k_daily_owid = case_when(!is.na(excess_per_million_weekly_filled) ~ excess_per_million_weekly_filled/(7*10), 
                                                !is.na(excess_per_million_monthly_filled) ~ excess_per_million_monthly_filled/(time_len*10))) %>% 
  mutate(owid_econ_diff = excess_per_100k_daily_owid-excess_per_100k_daily_economist, 
         owid_econ_pdiff = (excess_per_100k_daily_owid-excess_per_100k_daily_economist)/excess_per_100k_daily_economist)

#### Export data ####
#### Format
Exmort_clean <- select(Exmort_full, country, date, excess_per_million_weekly, excess_per_million_monthly, excess_per_100k_daily_owid, 
                       excess_per_100k_daily_economist)

write_csv(Exmort_clean, "data/clean/exmort.csv")

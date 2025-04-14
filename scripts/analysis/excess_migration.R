#### Excess migration ####
#### Project migration we would have seen after March, 2020 (using a simple time series model)
#### Compare excess to actual observed migration

##### Setup ####
rm(list = ls())
setwd(here::here())
# Load packages
source("scripts/utilities/load_packages.R")
library(forecast)

### Load data
setwd("data/clean_data")
Data <- read_csv("full_unadj.csv") %>% 
  # Create variable for expats (not log transformed)
  mutate(expats = exp(log_expats)) 

#### Clean data ####
#### Create time series of outcome- log expats
Time_series <- Data %>% 
  # Cut out all variables besides time & outcome
  select(o_d_pair, date, expats) %>% 
  # Fill in missing dates with NA
  full_join(expand_grid(o_d_pair = unique(Data$o_d_pair), 
                        date = seq.Date(from = ymd("2019-05-01"), to = ymd("2020-06-01"), by = "month")-14)) %>% 
  .[order(.$o_d_pair, .$date), ] %>% 
  # Split into list by origin-destination pairings
  split(.$o_d_pair) %>% 
  # Convert to list of time series objects
  map(~ ts(.$expats, start = c(2019, 5), frequency = 12))

#### Analysis ####
#### Project migrants after March 2020
Migrants_expected <- Time_series %>% 
  map(~ window(., end = c(2020, 3))) %>% 
  map(~ auto.arima(.)) %>% 
  map(~ forecast(., h=3)) %>% 
  map(~ as_tibble(., rownames = "year_month")) %>%
  bind_rows(.id = "o_d_pair")

#### Combine back with actual migration and calculate difference
Excess_migration <- Migrants_expected %>% 
  # Convert year-month to date & only keep the variables I need
  mutate(date = as.Date(as.yearmon(year_month))-14) %>% 
  select(o_d_pair, date, `Point Forecast`, `Lo 95`, `Hi 95`) %>% 
  # Combine back with raw data
  left_join(Data, .) %>%
  # Only keep needed variables & last time period
  select(origin, country, o_d_pair, od_id, date, `Point Forecast`, `Lo 95`, `Hi 95`, expats) %>% 
  filter(date == ymd("2020-05-18")) %>% 
  ## Calculate excess migrants
  mutate(excess_migrants = expats - `Point Forecast`, 
         excess_migrants_lo = expats - `Lo 95`, 
         excess_migrants_hi = expats - `Hi 95`)

#### Export excess migration ####
write_csv(Excess_migration, "excess_migration.csv")

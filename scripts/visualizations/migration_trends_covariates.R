#### Migration trends & covariates plots ####
#### Plot trends of expats -> different levels of covariates

##### Setup ####
rm(list = ls())
setwd(here::here())
# Load packages
source("scripts/utilities/load_packages.R")
library(ggrepel)
library(cowplot)
library(grid)
library(gridExtra)
library(scales)

### Load data
setwd("data/clean_data")
# Main data
Data <- read_csv("full_unadj.csv") %>% 
  # Create variable for expats (not log transformed)
  mutate(expats = exp(log_expats)) 
# Excess mortality
ExM <- read_csv("exmort.csv")
# Stringency index
OxCGRT <- read_csv("OxCGRT_clean.csv")

#### Clean data ####
## Only keep needed variables & dates from main data
Data_clean <- select(Data, origin, country, o_d_pair, od_id, date, log_expats, expats) %>% 
  filter(date >= ymd("2020-02-16")) %>% 
  # expand to include all dates from 2/16 -> 5/18
  full_join(expand_grid(unique(select(Data, origin, country, o_d_pair, od_id)), 
                        date = seq.Date(ymd("2020-02-16"), ymd("2020-05-18"), "day")))

#### Excess mortality
## Create excess mortality best estimate (use owid, if dont have use economist)
ExM_clean <- ExM %>% 
  mutate(excess_per_100k_daily_best = case_when(!is.na(excess_per_100k_daily_owid) ~ excess_per_100k_daily_owid, 
                                                is.na(excess_per_100k_daily_owid) ~ excess_per_100k_daily_economist))
# Select needed dates & needed variables
ExM_clean <- right_join(ExM_clean, expand_grid(country = unique(Data$country), 
                                               date = seq.Date(ymd("2020-02-16"), ymd("2020-05-18"), "day"))) %>% 
  select(country, date, excess_per_100k_daily_best)

#### Stringency index
String_clean <-  OxCGRT %>% 
  # Drop unused variables & rename others
  mutate(country = countrycode(CountryCode, "iso3c", "iso2c")) %>% 
  select(country, date = Date, stringency_index = Stringency_new)
# Select needed dates & needed countries
String_clean <- right_join(String_clean, expand_grid(country = unique(Data$country), 
                                               date = seq.Date(ymd("2020-02-16"), ymd("2020-05-18"), "day")))

### Combine covariates & expat data from main
Expats_covars <- full_join(Data_clean, ExM_clean) %>% 
  full_join(String_clean)

## Order & fill in blanks
# Order
Expats_covars <- Expats_covars[order(Expats_covars$origin, Expats_covars$country, Expats_covars$date), ] 
# Fill in log expat blanks
Expats_covars <- setDT(Expats_covars)[, log_expats := na.locf(log_expats, na.rm = F, fromLast = F), origin:country]
# Fill in expat blanks
Expats_covars <- setDT(Expats_covars)[, expats := na.locf(expats, na.rm = F, fromLast = F), origin:country]

## Create separate variable for destination country label (to only label 1st observation in plot)
Expats_covars <- Expats_covars %>% 
  mutate(country_label = if_else(date == min(Expats_covars$date), country, NA_character_))

### Create subsets of data of top 10 destinations for each origin
Expats_covars <- filter(Expats_covars, date == min(Expats_covars$date)) %>% 
  group_by(origin) %>% 
  mutate(rank = rank(-expats)) %>% 
  ungroup() %>% 
  select(origin:od_id, rank) %>% 
  full_join(Expats_covars, .)

##### Plot ####
# Setup
setwd(here::here())
setwd("figures")
#### Excess mortality
filter(Expats_covars, rank <= 10) %>% 
  ggplot(aes(x = date, y = expats, color = excess_per_100k_daily_best, group = country)) + 
  facet_grid(cols = vars(origin)) +
  geom_line() + 
  scale_y_continuous(name = "Migrant Stock (Estimated)", trans = log_trans(), breaks = c(10000, 30000, 100000, 300000, 1000000), labels = scales::comma) + 
  geom_label_repel(aes(label = country_label), nudge_x = -7, na.rm = T, color = "black") + 
  scale_x_date(name = "Date") + 
  scale_color_distiller(name = "Excess Mortality (per 100,000)", palette = "RdBu") + 
  theme_dark() + 
  theme(axis.title = element_text(face="bold"))
### Export
ggsave("excess_mortality_expats_trends.png", width = 500, height = 500, units = "px", scale = 6)

#### Stringency index
filter(Expats_covars, rank <= 10) %>% 
  ggplot(aes(x = date, y = expats, color = stringency_index, group = country)) + 
  facet_grid(cols = vars(origin)) +
  geom_line() + 
  scale_y_continuous(name = "Migrant Stock (Estimated)", trans = log_trans(), breaks = c(10000, 30000, 100000, 300000, 1000000), labels = scales::comma) + 
  geom_label_repel(aes(label = country_label), nudge_x = -7, na.rm = T, color = "black") + 
  scale_x_date(name = "Date") + 
  scale_color_distiller(name = "Stringency Index", palette = "RdBu") + 
  theme_dark() + 
  theme(axis.title = element_text(face="bold"))
### Export
ggsave("stringency_expats_trends.png", width = 500, height = 500, units = "px", scale = 6)

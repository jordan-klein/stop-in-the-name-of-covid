##### Clean Stringency Index Data ####
#### Recalculates the Oxford COVID-19 Government Response Tracker Stringency Index excluding international travel controls
### Calculate stringency index using components C1-C7 & H1, excluding C8
## See: 
# https://github.com/OxCGRT/covid-policy-tracker
# https://github.com/OxCGRT/covid-policy-tracker/tree/master/data
# https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md

#### Setup ####
rm(list = ls())
setwd(here::here())
# Load packages
source("scripts/utilities/load_packages.R")

## Load data (see https://github.com/OxCGRT/covid-policy-tracker#getting-data-from-this-github-repository)
# Raw data on Oxford COVID-19 Government Response Tracker indicators
OxCGRT <- getURL("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv") %>% 
  read_csv()

#### Calculate sub-index scores for each indicator ####
#### See: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md#calculating-sub-index-scores-for-each-indicator
OxCGRT_ind <- mutate(OxCGRT, 
                            C1 = if_else(`C1_School closing` == 0, 0, 
                                           100*(`C1_School closing`-.5*(1-C1_Flag))/3), 
                            C2 = if_else(`C2_Workplace closing` == 0, 0, 
                                         100*(`C2_Workplace closing`-.5*(1-C2_Flag))/3), 
                            C3 = if_else(`C3_Cancel public events` == 0, 0, 
                                         100*(`C3_Cancel public events`-.5*(1-C3_Flag))/2), 
                            C4 = if_else(`C4_Restrictions on gatherings` == 0, 0, 
                                         100*(`C4_Restrictions on gatherings`-.5*(1-C4_Flag))/4), 
                            C5 = if_else(`C5_Close public transport` == 0, 0, 
                                         100*(`C5_Close public transport`-.5*(1-C5_Flag))/2), 
                            C6 = if_else(`C6_Stay at home requirements` == 0, 0, 
                                         100*(`C6_Stay at home requirements`-.5*(1-C6_Flag))/3), 
                            C7 = if_else(`C7_Restrictions on internal movement` == 0, 0, 
                                         100*(`C7_Restrictions on internal movement`-.5*(1-C7_Flag))/2), 
                            C8 = 100*(`C8_International travel controls`)/4, 
                            H1 = if_else(`H1_Public information campaigns` == 0, 0, 
                                         100*(`H1_Public information campaigns`-.5*(1-H1_Flag))/2)) %>% 
  ## Only keep national level
  filter(Jurisdiction == "NAT_TOTAL") %>%
  ## Only keep needed variables
  select(CountryName, CountryCode, Date, `C1_School closing`:`C8_International travel controls`, 
         `H1_Public information campaigns`, H1_Flag, C1:H1, StringencyIndex, StringencyIndexForDisplay, ConfirmedCases, ConfirmedDeaths)

#### Calculate stringency index ####
#### See: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md#methodology-for-calculating-indices
## Original w/travel restrictions (C1-C8, H1)
# NA if at least 2 missing sub-indices
OxCGRT_ind <- OxCGRT_ind %>% 
  rowwise() %>% 
  mutate(Stringency_OG = round(sum(c_across(C1:H1), na.rm = T)/9, 2), 
         Stringency_OG_nas = sum(is.na(c_across(C1:H1)))) %>% 
  mutate(Stringency_OG = if_else(Stringency_OG_nas >= 2, NaN, Stringency_OG)) %>% 
  select(-Stringency_OG_nas)

# Check to make sure this is calculating the same thing as Oxford's stringency index
table(OxCGRT_ind$Stringency_OG == OxCGRT_ind$StringencyIndex, useNA = "ifany")
Ox_check <- filter(OxCGRT_ind, (is.na(Stringency_OG) | is.na(StringencyIndex)) & !(is.na(Stringency_OG) & is.na(StringencyIndex)))
# Discrepancies with very recent data are expected/not a problem (only 1 when code originally ran for Poland from previous day)

## Version w/o travel restrictions (C1-C7, H1)
# NA if at least 2 missing sub-indices
OxCGRT_ind <- OxCGRT_ind %>% 
  rowwise() %>% 
  mutate(Stringency_new = round(sum(c_across(c(C1:C7,H1)), na.rm = T)/8, 2), 
         Stringency_new_nas = sum(is.na(c_across(c(C1:C7,H1))))) %>% 
  mutate(Stringency_new = if_else(Stringency_new_nas >= 2, NaN, Stringency_new)) %>% 
  select(-Stringency_new_nas)

#### Clean data ####
## Only keep some variables
OxCGRT_clean <- select(OxCGRT_ind, CountryName, CountryCode, Date, C1:H1, StringencyIndex, Stringency_new, ConfirmedCases, ConfirmedDeaths) %>% 
  ungroup() %>%
  # Convert date variable to ymd format
  mutate(Date = ymd(Date))

#### Export data ####
setwd("data/clean_data")
write_csv(OxCGRT_clean, "OxCGRT_clean.csv")

#### Create master dataset ####
### packages
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(countrycode)

#### Load data ####
### FB
FB <- gzfile("FB_fulltimeseries.csv.gz") %>% 
  read_csv()
### Official migrant stock
EU_US_stock <- read_csv("EU_US_migrant_stock.csv")
UN_stock <- read_csv("UN_migrant_stock.csv")
### Travel restrictions
Travel <- read_csv("travel_restriction_matrix_updated.csv")
### Excess mortality
ExM <- read_csv("exmort.csv")
### Stringency
String <- read_csv("covid-stringency-index.csv")
## Get list of countries
clist <- c("US","CA","GB","ES","FR","IT","BE","NL","DE","TR","AE","EG","MA","DZ","TN","MR","ML","BF","SN","GM","GN","CI","BJ","GA")

#### Clean data ####
#### FB
### Select origins & destinations
FB_clean <- filter(FB, grepl(paste0(clist, collapse = "|"), Key)) %>% 
  filter(grepl("All|Ivore|Ivoire|Algeria|Morocco|Senegal", citizenship))
### Fix Ivore/Ivoire discrepancy
CI_fix <- filter(FB_clean, grepl("Ivore", citizenship)) %>% 
  full_join(filter(FB_clean, grepl("Ivoire", citizenship)), 
            by = c("Month", "Key", "genders", "age"), 
            suffix = c(".ivore", ".ivoire")) %>% 
  select(Month, Key, genders, age, citizenship.ivore, citizenship.ivoire, 
         mau_audience.ivore, mau_audience.ivoire) %>% 
  mutate(mau_audience = case_when(mau_audience.ivore > 0 ~ mau_audience.ivore, 
                                  mau_audience.ivoire > 0 ~ mau_audience.ivoire)) %>% 
  mutate(citizenship = citizenship.ivoire) %>% 
  select(Month, Key, mau_audience, genders, age, citizenship)
# Put back in main data
FB_clean <- filter(FB_clean, !grepl("Ivore|Ivoire", citizenship)) %>% 
  bind_rows(CI_fix)

## Cut out obs where origin = destination
FB_fixed <- filter(FB_clean, !(grepl("Algeria", citizenship) & Key == "DZ") & 
                     !(grepl("Morocco", citizenship) & Key == "MA") & 
                     !(grepl("Senegal", citizenship) & Key == "SN") & 
                     !(grepl("Ivoire", citizenship) & Key == "CI"))

## Rename vars & calculate 14 day lag of FB data
FB_fixed <- mutate(FB_fixed, date = Month-14) %>% 
  select(country = Key, date, mau_audience:citizenship)

## Convert -1 to NA
FB_fixed <- mutate(FB_fixed, mau_audience = ifelse(mau_audience == -1, NA, mau_audience))


### Calculate each citizenship as proportion of all expats
FB_processed <- FB_fixed %>%
  filter(citizenship == "Expats (All)") %>% 
  rename(allexpats = mau_audience) %>% 
  select(-citizenship) %>% 
  full_join(FB_fixed) %>% 
  mutate(prop_of_expats = mau_audience/allexpats)

### Extract just migrants rows
FB_migrants <- filter(FB_processed, genders == "both" & age == "13-" & 
                        grepl("Algeria|Morocco|Senegal|Ivoire", citizenship)) %>% 
  mutate(origin = case_when(grepl("Algeria", citizenship) ~ "DZ", 
                            grepl("Morocco", citizenship) ~ "MA", 
                            grepl("Senegal", citizenship) ~ "SN", 
                            grepl("Ivoire", citizenship) ~ "CI")) %>% 
  select(country, origin, date, mau_audience, allexpats, prop_of_expats)

## Quick plots
ggplot(data = FB_migrants, aes(x = date, y = mau_audience, color = origin)) + geom_line() + facet_wrap(~country)
ggplot(data = FB_migrants, aes(x = date, y = prop_of_expats, color = origin)) + geom_line() + facet_wrap(~country)

#### Create date key
cutoff_dates <- c(min(FB_migrants$date)-1, unique(FB_migrants$date), ymd("2020-06-01"))

date_key <- tibble(duration = (cutoff_dates-lag(cutoff_dates))[-1], 
                   date = cutoff_dates[-1], time_interval = c(1:length(cutoff_dates[-1])))

##### Travel
head(Travel)
#### Clean
Travel_clean <- mutate(Travel, date = mdy(date), status = as.factor(status)) %>% 
  rename(country = destination)
# Fill in dates back to start of FB data (2019-04-17)
Travel_clean <- full_join(Travel_clean, 
                          expand_grid(unique(Travel_clean[, 1:2]), 
                                      date = seq.Date(min(FB_processed$date), max(Travel_clean$date), "day"))) %>% 
  mutate(status = if_else(is.na(status), status[1], status)) %>% 
  # Combine w date key
  full_join(date_key)
# Order & fill in blanks
Travel_clean <- Travel_clean[order(Travel_clean$origin, Travel_clean$country, Travel_clean$date), ] 
Travel_clean <- setDT(Travel_clean)[, duration := na.locf(duration, na.rm = F, fromLast = T), origin:country]
Travel_clean <- setDT(Travel_clean)[, time_interval := na.locf(time_interval, na.rm = F, fromLast = T), origin:country]

### Calculate restriction indices
Travel_calculated <- Travel_clean %>% 
  group_by(origin, country, time_interval) %>% 
  count(status, .drop = F) %>% 
  pivot_wider(names_from = status, values_from = n) %>% 
  mutate(travel_rest_ind = red+.5*yellow) %>% 
  full_join(Travel_clean, .) %>% 
  mutate(travel_rest_ind = travel_rest_ind/as.numeric(duration))

##### Excess Mortality
head(ExM)
#### Clean- create excess mortality best estimate (use owid, if dont have use economist)
ExM_clean <- ExM %>% 
  mutate(excess_per_100k_daily_best = case_when(!is.na(excess_per_100k_daily_owid) ~ excess_per_100k_daily_owid, 
                                                is.na(excess_per_100k_daily_owid) ~ excess_per_100k_daily_economist))
# Fill in dates back to start of FB data (2019-04-17) & combine w date key
ExM_clean <- full_join(ExM_clean, expand_grid(country = unique(ExM$country), 
                                   date = seq.Date(min(FB_processed$date), max(ExM_clean$date), "day"))) %>% 
  full_join(date_key)
# Order & fill in blanks
ExM_clean <- ExM_clean[order(ExM_clean$country, ExM_clean$date), ] 
ExM_clean <- setDT(ExM_clean)[, duration := na.locf(duration, na.rm = F, fromLast = T), country]
ExM_clean <- setDT(ExM_clean)[, time_interval := na.locf(time_interval, na.rm = F, fromLast = T), country]

### Calculate w/in interval excess mortality (sum)
ExM_calculated <- ExM_clean %>% 
  group_by(country, time_interval) %>% 
  summarise(excess_mortality_per_100k = sum(excess_per_100k_daily_best)) %>% 
  full_join(ExM_clean, .)

##### Stringency
head(String)
#### Clean- select countries & dates (earlier than 2020-06-01)
String_clean <- String %>% 
  mutate(country = countrycode(Code, "iso3c", "iso2c")) %>% 
  filter(grepl(paste0(clist, collapse = "|"), country)) %>% 
  select(country, date = Day, stringency_index) %>% 
  filter(date <= ymd("2020-06-01"))
# Fill in dates back to start of FB data (2019-04-17) & combine w date key
String_clean <- full_join(String_clean, expand_grid(country = unique(String_clean$country), 
                                                    date = seq.Date(min(FB_processed$date), max(ExM_clean$date), "day"))) %>% 
  full_join(date_key)
# Order & fill in blanks
String_clean <- String_clean[order(String_clean$country, String_clean$date), ] 
String_clean <- setDT(String_clean)[, duration := na.locf(duration, na.rm = F, fromLast = T), country]
String_clean <- setDT(String_clean)[, time_interval := na.locf(time_interval, na.rm = F, fromLast = T), country]
## Insert 0s to fill out time interval 10 (Jan 19th & 20th 2020)
String_clean <- String_clean %>% 
  mutate(stringency_index = if_else(time_interval == 10 & is.na(stringency_index), 0, stringency_index))

### Calculate w/in interval stringency (mean)
String_calculated <- String_clean %>% 
  group_by(country, time_interval) %>% 
  summarise(string_mean = mean(stringency_index)) %>% 
  full_join(String_clean, .)

##### Create master dataset ####
#### Migrants
Master_migrants <- full_join(FB_migrants, Travel_calculated) %>% 
  full_join(ExM_calculated) %>% 
  full_join(String_calculated) %>%
  select(country:date, time_interval, duration, mau_audience:prop_of_expats, status, travel_rest_ind, 
         excess_per_100k_daily_best, excess_mortality_per_100k, stringency_index, string_mean)
Master_migrants <- Master_migrants[order(Master_migrants$origin, Master_migrants$country, Master_migrants$date), ]

#### Full dataset
Master_full <- full_join(FB_processed, 
                         full_join(Travel_calculated, 
                                   expand_grid(citizenship = unique(FB_processed$citizenship), 
                                               age = unique(FB_processed$age), 
                                               genders = unique(FB_processed$genders), 
                                               date = unique(Travel_calculated$date)))) %>% 
  full_join(ExM_calculated) %>% 
  full_join(String_calculated) %>% 
  select(country, date, time_interval, duration, citizenship, age, genders, mau_audience, allexpats, prop_of_expats, 
         origin, status, travel_rest_ind, excess_per_100k_daily_best, excess_mortality_per_100k, stringency_index, string_mean)
Master_full <- Master_full[order(Master_full$country, Master_full$date), ]

#### Export data ####
### Migrants
write_csv(Master_migrants, "Master_migrants.csv")

### Full
#### Write file ####
gzfile("Master_full.csv.gz") %>%
  write_csv(Master_full, file = .)

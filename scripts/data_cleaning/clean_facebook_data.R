#### Process Facebook expat data ####
#### Use preprocessed Facebook timeseries data
#### Produce: Processed data with monthly active users by gender, age & citizenship in countries of interest
### Facebook expats per 100,000 in origin/destination country pairs (unadjusted for Facebook penetration)
#### Setup ####
rm(list = ls())
# Load packages
source("scripts/utilities/load_packages.R")

# Set working directory to clean data
setwd("data")

# Load
FB <- gzfile("preprocessed/FB_fulltimeseries.csv.gz") %>% 
  read_csv()

# List of countries included in analysis
clist <- c("US","CA","GB","ES","FR","IT","BE","NL","DE","TR","AE","EG","MA","DZ","TN","MR","ML","BF","SN","GM","GN","CI","BJ","GA")

#### Clean data ####
## Select origins & destinations
FB_clean <- filter(FB, grepl(paste0(clist, collapse = "|"), Key)) %>% 
  filter(grepl("All|Ivore|Ivoire|Algeria|Morocco|Senegal", citizenship))
## Fix Ivore/Ivoire discrepancy
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

#### Process data ####
#### Monthly active users by gender, age & citizenship in countries of interest
### Calculate each citizenship as proportion of all expats
FB_processed <- FB_fixed %>%
  filter(citizenship == "Expats (All)") %>% 
  rename(allexpats = mau_audience) %>% 
  select(-citizenship) %>% 
  full_join(FB_fixed) %>% 
  mutate(prop_of_expats = mau_audience/allexpats)

#### Facebook expats per 100,000 in origin/destination country pairs (unadjusted for Facebook penetration)
### Extract just migrants rows
FB_migrants <- filter(FB_processed, genders == "both" & age == "13-" & 
                        grepl("Algeria|Morocco|Senegal|Ivoire", citizenship)) %>% 
  mutate(origin = case_when(grepl("Algeria", citizenship) ~ "DZ", 
                            grepl("Morocco", citizenship) ~ "MA", 
                            grepl("Senegal", citizenship) ~ "SN", 
                            grepl("Ivoire", citizenship) ~ "CI")) %>% 
  select(country, origin, date, mau_audience, allexpats, prop_of_expats) %>% 
  ## Calculate expats per 100,000
  mutate(expats_per_100k = prop_of_expats*100000)
  
#### Export ####
setwd("clean_data/facebook")

### Full processed facebook data
write_csv(FB_processed, "FB_full_processed.csv")

### (Unadjusted) Facebook expats per 100,000
write_csv(FB_migrants, "FB_expats_per100k_unadj.csv") 

#### Travel restrictions & migration trends plots ####
#### Plot trends of expats -> different levels of treatment

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
# Travel restrictions
Travel <- read_csv("travel_restriction_matrix_updated.csv")

#### Clean data ####
## Only keep needed variables & dates from main data
Data_clean <- select(Data, origin, country, o_d_pair, od_id, date, log_expats, expats) %>% 
  filter(date >= ymd("2020-02-16")) %>% 
  # expand to include all dates from 2/16 -> 5/18
  full_join(expand_grid(unique(select(Data, origin, country, o_d_pair, od_id)), 
                        date = seq.Date(ymd("2020-02-16"), ymd("2020-05-18"), "day")))

### Travel restriction data
## Convert date to date format & travel restrictions to factor
Travel_clean <- mutate(Travel, date = mdy(date), status = as.factor(status)) %>% 
  rename(country = destination)

## Fill in dates between 2020-03-08 back to 2020-02-16 
# This was the first month restrictions were in place, we measure the treatment over the whole month & assume
# if no restrictions for 1st date available (2020-03-08) -> no restrictions back to 2020-02-16
Travel_clean <- right_join(Travel_clean, 
                          expand_grid(unique(select(Data, origin, country, o_d_pair, od_id)), 
                                      date = seq.Date(ymd("2020-02-16"), ymd("2020-05-18"), "day"))) %>% 
  mutate(status = if_else(is.na(status), status[1], status)) 

### Combine travel restriction & expat data from main
Expats_travel <- full_join(Data_clean, Travel_clean)

## Order & fill in blanks
# Order
Expats_travel <- Expats_travel[order(Expats_travel$origin, Expats_travel$country, Expats_travel$date), ] 
# Fill in log expat blanks
Expats_travel <- setDT(Expats_travel)[, log_expats := na.locf(log_expats, na.rm = F, fromLast = F), origin:country]
# Fill in expat blanks
Expats_travel <- setDT(Expats_travel)[, expats := na.locf(expats, na.rm = F, fromLast = F), origin:country]

## Create separate variable for destination country label (to only label 1st observation in plot)
Expats_travel <- Expats_travel %>% 
  mutate(country_label = if_else(date == min(Expats_travel$date), country, NA_character_))

### Create subsets of data of top 10 destinations for each origin
Expats_travel <- filter(Expats_travel, date == min(Expats_travel$date)) %>% 
  group_by(origin) %>% 
  mutate(rank = rank(-expats)) %>% 
  ungroup() %>% 
  select(origin:od_id, rank) %>% 
  full_join(Expats_travel, .) %>% 
  mutate(status = case_when(status == "green" ~ "Green", status == "yellow" ~ "Yellow", status == "red" ~ "Red"))

##### Plot ####
filter(Expats_travel, rank <= 10) %>% 
  ggplot(aes(x = date, y = expats, color = status, group = country)) + 
  facet_grid(cols = vars(origin)) +
  geom_line() + 
  scale_color_manual(values = c("Green" = "Green", "Yellow" = "Yellow", "Red" = "Red"), name = "Travel Restrictions") + 
  scale_y_continuous(name = "Migrant Stock (Estimated)", trans = log_trans(), breaks = c(10000, 30000, 100000, 300000, 1000000), labels = scales::comma) + 
  geom_label_repel(aes(label = country_label), nudge_x = -7, na.rm = T, color = "black") + 
  scale_x_date(name = "Date") + theme_dark() + 
  theme(axis.title = element_text(face="bold"))

#### Export graph ####
setwd(here::here())
setwd("figures")
ggsave("travel_restrictions_expats_trends.png", width = 500, height = 500, units = "px", scale = 6)

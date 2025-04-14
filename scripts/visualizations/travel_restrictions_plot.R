#### Create visualization of treatment (travel restriction) variability over time ####
#### Color-coded treatment

#### Setup ####
rm(list = ls())
setwd(here::here())
# Load packages
source("scripts/utilities/load_packages.R")
library(grid)
library(gridExtra)

# List of countries included in analysis
clist <- c("US","CA","GB","ES","FR","IT","BE","NL","DE","TR","AE","EG","MA","DZ","TN","MR","ML","BF","SN","GM","GN","CI","BJ","GA")

### Load data
setwd("data/clean_data")
Travel <- read_csv("travel_restriction_matrix_updated.csv")

#### Clean data ####
Travel_clean <- mutate(Travel, date = mdy(date), status = as.factor(status)) %>% 
  # Select only included destination countries
  filter(grepl(paste0(clist, collapse = "|"), destination) & destination != "SN") %>%
  # Create origin-destionation pairing
  mutate(d_o_pair = paste0(destination, "-", origin))

## Fill in dates between 2020-03-08 back to 2020-02-16 
# This was the first month restrictions were in place, we measure the treatment over the whole month & assume
# if no restrictions for 1st date available (2020-03-08) -> no restrictions back to 2020-02-16
Travel_clean <- right_join(Travel_clean, 
                           expand_grid(unique(Travel_clean[, c(1:2,5)]), 
                                       date = seq.Date(ymd("2020-02-16"), ymd("2020-05-18"), "day"))) %>% 
  mutate(status = if_else(is.na(status), status[1], status)) %>% 
  mutate(status = case_when(status == "green" ~ "Green", status == "yellow" ~ "Yellow", status == "red" ~ "Red"))

# Order by destination- then origin
Travel_clean <- Travel_clean[order(Travel_clean$destination, Travel_clean$origin), ] 
  
#### Create graph ####
ggplot(Travel_clean, aes(date, origin)) + geom_tile(aes(fill=status), color = "black") + 
  scale_fill_manual(values = c("Green" = "Green", "Yellow" = "Yellow", "Red" = "Red"), name = "Travel Restrictions") + 
  facet_grid(destination ~ ., scales = "free", switch = "both") + 
  theme(panel.spacing.y = unit(.05,"cm"), strip.placement = "outside", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.title = element_text(face="bold")) + 
  scale_y_discrete(limits = rev, expand = c(0,0), name = "Destination-Origin Pairwise Restrictions") +
  scale_x_date(expand = c(0,0), name = "Date") + 
  geom_vline(xintercept = c(ymd("2020-02-16"), ymd("2020-03-18"), ymd("2020-04-17"), ymd("2020-05-18")), 
             color = "darkblue", size = 1)

#### Export graph ####
setwd(here::here())
setwd("figures")
ggsave("travel_restrictions.png", width = 500, height = 500, units = "px", scale = 6)

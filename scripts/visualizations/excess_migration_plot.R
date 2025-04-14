#### Create visualization of excess migration (actual-expected expats) ####

#### Setup ####
rm(list = ls())
setwd(here::here())
# Load packages
source("scripts/utilities/load_packages.R")

### Load data
setwd("data/clean_data")
Excess_migration <- read_csv("excess_migration.csv")

#### Create graph ####
#### Create index of destination countries
dest_index <- tibble(country = unique(Excess_migration$country)) %>% 
  .[order(.$country), ] %>% 
  mutate(index = index(country))
## But pack into data to id which countries going into 1st vs 2nd graph
Plot_data <- full_join(Excess_migration, dest_index) %>% 
  mutate(plot = case_when(index <= 12 ~ 1, index >= 13 ~ 2))

#### Create signed-log axes
weird <- scales::trans_new("signed_log",
                           transform=function(x) {
                             if_else(x == 0, 0, sign(x)*log(abs(x)))
                           },inverse=function(x) sign(x)*exp(abs(x)))

#### Plot
ggplot(Plot_data, aes(x = country, y = excess_migrants, color = origin)) + 
  geom_pointrange(position = position_dodge(width = .5), aes(ymax =excess_migrants_hi, ymin = excess_migrants_lo)) + 
  geom_hline(yintercept = 0) +
  coord_flip() + scale_x_discrete(limits = rev, name = "Destination Country") + 
  scale_y_continuous(trans = weird, 
                     breaks = c(-100000, -10000, -1000, -100, -10, 0, 10, 100, 1000, 10000, 100000), 
                     labels = scales::comma, name = "Excess Migrants") + 
  scale_color_discrete(name = "Origin Country") + 
  theme(axis.title = element_text(face="bold"))

#### Export graph ####
setwd(here::here())
setwd("figures")
ggsave("excess_migration.png", width = 500, height = 500, units = "px", scale = 6)

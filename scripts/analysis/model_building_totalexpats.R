#### Build models ####
#### Building models of facebook expats as a function of the travel restriction treatment w/ covariates
### ** Using total expats scaled for expat population from official sources***
#### Not enough observation periods to properly do AB with lagged excess mortality (or official deaths)- mtest of 2nd order serial correlation not defined

#### Setup ####
rm(list = ls())
setwd(here::here())
# Load packages
source("scripts/utilities/load_packages.R")
library(plm)
library(lmtest)
library(pdynmc)
library(stargazer)

### Load data
setwd("data/clean_data")
Data <- read_csv("full_unadj.csv")
## Migrant stock official data
USEU_stock <- read_csv("EU_US_migrant_stock.csv")
UN_stock <- read_csv("UN_migrant_stock.csv")

#### Combine Data w/ Migrant stock data ####
##### Expats from o in country d
#### Clean migrant stock data- use US & EU data instead of UN data for those countries
Stock <- filter(UN_stock, !(country_code %in% unique(USEU_stock$country_code))) %>% 
  select(-country_name) %>% 
  bind_rows(USEU_stock)

#### Combine w/ master data
Data <- mutate(Data, Year = year(date)) %>%
  left_join(Stock, by = c("country" = "country_code", "Year")) %>% 
  mutate(expats = log(expats_per_100k/100000*population))

#### Run models ####
#### Simple- no covariates
simple_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
       include.y = T, varname.y = "expats", lagTerms.y = 1,
       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
       varname.reg.fur = "travel_rest_ind", lagTerms.reg.fur = c(0), 
       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
       opt.meth = "none")
summary(simple_model)
mtest.fct(simple_model)
jtest.fct(simple_model)
wald.fct(simple_model, param = "all")

#### Covariates = W (pandemic dummy)
dummy_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats", lagTerms.y = 1,
                       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                       varname.reg.fur = c("travel_rest_ind", "W"), lagTerms.reg.fur = rep(0, 2), 
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "none")
summary(dummy_model)
mtest.fct(dummy_model)
jtest.fct(dummy_model)
wald.fct(dummy_model, param = "all")

#### Covariates = excess mortality
exmort_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                      use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                      include.y = T, varname.y = "expats", lagTerms.y = 1,
                      fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                      varname.reg.fur = c("travel_rest_ind", "W", 
                                          "excess_mortality_per_100k.o", "excess_mortality_per_100k.d"), 
                      lagTerms.reg.fur = c(0, 0, 0, 0), 
                      w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                      opt.meth = "none")
summary(exmort_model)
mtest.fct(exmort_model)
jtest.fct(exmort_model)
wald.fct(exmort_model, param = "all")

#### Covariates = stringency
string_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats", lagTerms.y = 1,
                       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                       varname.reg.fur = c("travel_rest_ind", "W", 
                                           "string_mean.o", "string_mean.d"), 
                       lagTerms.reg.fur = c(0, 0, 0, 0), 
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "none")
summary(string_model)
mtest.fct(string_model)
jtest.fct(string_model)
wald.fct(string_model, param = "all")

#### Mobility
# Doesn't pass J-test of overidentifying restrictions
mobility_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats", lagTerms.y = 1,
                       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                       varname.reg.fur = c("travel_rest_ind", "W", 
                                           "mobility_mean.o", "mobility_mean.d"), 
                       lagTerms.reg.fur = c(0, 0, 0, 0), 
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "none")
summary(mobility_model)
mtest.fct(mobility_model)
jtest.fct(mobility_model)
wald.fct(mobility_model, param = "all")


#### Cases
# Doesn't pass Wald test or mtest of 2nd order serial correlation
cases_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats", lagTerms.y = 1,
                       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                       varname.reg.fur = c("travel_rest_ind", "W", 
                                           "cases_per_100k.o", "cases_per_100k.d"), 
                       lagTerms.reg.fur = c(0, 0, 0, 0), 
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "none")
summary(cases_model)
mtest.fct(cases_model)
jtest.fct(cases_model)
wald.fct(cases_model, param = "all")

#### Deaths
death_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats", lagTerms.y = 1,
                       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                       varname.reg.fur = c("travel_rest_ind", "W", 
                                           "deaths_per_100k.o", "deaths_per_100k.d"), 
                       lagTerms.reg.fur = c(0, 0, 0, 0), 
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "none")
summary(death_model)
mtest.fct(death_model)
jtest.fct(death_model)
wald.fct(death_model, param = "all")

#### Run models of combined covariates ####
#### Full model
# Satisfied AB assumptions
full_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                     use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                     include.y = T, varname.y = "expats", lagTerms.y = 1,
                     fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                     varname.reg.fur = c("travel_rest_ind", "W", 
                                         "excess_mortality_per_100k.o", "excess_mortality_per_100k.d", 
                                         "cases_per_100k.o", "cases_per_100k.d", 
                                         "deaths_per_100k.o", "deaths_per_100k.d", 
                                         "string_mean.o", "string_mean.d", 
                                         "mobility_mean.o", "mobility_mean.d"), 
                     lagTerms.reg.fur = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                     opt.meth = "none")
summary(full_model)
mtest.fct(full_model)
jtest.fct(full_model)
wald.fct(full_model, param = "all")

#### Correlation matrix
data.frame(Data$travel_rest_ind, Data$string_mean.d, Data$string_mean.o, 
           Data$mobility_mean.d, Data$mobility_mean.o, 
           Data$excess_mortality_per_100k.d, Data$excess_mortality_per_100k.o, 
           Data$deaths_per_100k.d, Data$deaths_per_100k.o, 
           Data$cases_per_100k.d, Data$cases_per_100k.o) %>% 
  cor(., use = "complete.obs")

## Cases, deaths, excess mortality highly correlated in destination 
# (not origin, many destination countries OECD w/ reliable reporting, origin = lower income)

## Stringency & mobility colinear
## -> justification for using original variables- all the new ones don't satisfy the AB assumptions 
# (except for deaths which is highly colinear w/ excess mortality)

# Original model (no lag)
og_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                      use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                      include.y = T, varname.y = "expats", lagTerms.y = 1,
                      fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                      varname.reg.fur = c("travel_rest_ind", "W", 
                                          "excess_mortality_per_100k.o", "excess_mortality_per_100k.d", 
                                          "string_mean.o", "string_mean.d"), 
                      lagTerms.reg.fur = c(0, 0, 0, 0, 0, 0), 
                      w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                      opt.meth = "none")
summary(og_model)
mtest.fct(og_model)
jtest.fct(og_model)
wald.fct(og_model, param = "all")

#### Try the model w/ different variables ####
# Fails mtest of 2nd order serial correlation
alt_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats", lagTerms.y = 1,
                       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                       varname.reg.fur = c("travel_rest_ind", "W", 
                                           "deaths_per_100k.o", "deaths_per_100k.d", 
                                           "cases_per_100k.o", "cases_per_100k.d", 
                                           "mobility_mean.o", "mobility_mean.d"), 
                       lagTerms.reg.fur = c(0, 0, 0, 0, 0, 0, 0, 0), 
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "none")
summary(alt_model)
mtest.fct(alt_model)
jtest.fct(alt_model)
wald.fct(alt_model, param = "all")

#### Conclusion = present all models + full model for log of expats as outcome ####

### Just try og model w/ mobility index- fails mtest of 2nd order serial correlation
og_wmob_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                   use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                   include.y = T, varname.y = "expats", lagTerms.y = 1,
                   fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                   varname.reg.fur = c("travel_rest_ind", "W", 
                                       "excess_mortality_per_100k.o", "excess_mortality_per_100k.d", 
                                       "string_mean.o", "string_mean.d", "mobility_mean.o", "mobility_mean.d"), 
                   lagTerms.reg.fur = c(0, 0, 0, 0, 0, 0, 0, 0), 
                   w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                   opt.meth = "none")
summary(og_wmob_model)
mtest.fct(og_wmob_model)
jtest.fct(og_wmob_model)
wald.fct(og_wmob_model, param = "all")

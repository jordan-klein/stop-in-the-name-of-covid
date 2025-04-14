#### Build models ####
#### Building models of facebook expats as a function of the travel restriction treatment w/ covariates
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

#### Run models ####
#### Simple- no covariates
simple_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
       include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
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
                       include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
                       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                       varname.reg.fur = c("travel_rest_ind", "W"), lagTerms.reg.fur = rep(0, 2), 
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "none")
summary(dummy_model)
mtest.fct(dummy_model)
jtest.fct(dummy_model)
wald.fct(dummy_model, param = "all")

#### Covariates = excess mortality
## (AB test NA w/ lags)
exmort_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                      use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                      include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
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
                       include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
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
                       include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
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
# Doesn't pass Wald test
cases_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
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
## (AB test NA- w/ lag)
death_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
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
# Fails J-test of overidentifying restrictions
full_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                     use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                     include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
                     fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                     varname.reg.fur = c("travel_rest_ind", "W", 
                                         "excess_mortality_per_100k.o", "excess_mortality_per_100k.d", 
                                         "cases_per_100k.o", "cases_per_100k.d", 
                                         "deaths_per_100k.o", "deaths_per_100k.d", 
                                         "string_mean.o", "string_mean.d", 
                                         "mobility_mean.o", "mobility_mean.d"), 
                     lagTerms.reg.fur = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0), 
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
# One new area = excess mortality w/ lag
oglag_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                     use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                     include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
                     fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                     varname.reg.fur = c("travel_rest_ind", "W", 
                                         "excess_mortality_per_100k.o", "excess_mortality_per_100k.d", 
                                         "string_mean.o", "string_mean.d"), 
                     lagTerms.reg.fur = c(0, 0, 1, 1, 0, 0), 
                     w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                     opt.meth = "none")
summary(oglag_model)
mtest.fct(oglag_model)
jtest.fct(oglag_model)
wald.fct(oglag_model, param = "all")

# Original model (no lag)
og_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                      use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                      include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
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

##** How to do AB test for 2-nd order serial correlation (mtest) w/ lagged excess mortality?
### Main models
## (Need to run a separate exmort w/ lag model first)
exmortlag_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                   use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                   include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
                   fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                   varname.reg.fur = c("travel_rest_ind", "W", 
                                       "excess_mortality_per_100k.o", "excess_mortality_per_100k.d"), 
                   lagTerms.reg.fur = c(0, 0, 1, 1), 
                   w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                   opt.meth = "none")
summary(exmortlag_model)
mtest.fct(exmortlag_model)
jtest.fct(exmortlag_model)
wald.fct(exmortlag_model, param = "all")

##
summary(simple_model)
summary(dummy_model)
summary(exmort_model)
summary(exmortlag_model)
summary(string_model)
summary(og_model)
summary(oglag_model)

### 1) Conclusions the same- much of reductions in expats can be explained by onset of the pandemic & restrictions in general (but not by health impacts)
### 2) To do- mtest for models w/ lagged excess mortality?

#### Try the model w/ different variables ####
# Both fail J-test of overidentifying restrictions
alt_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
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

altlag_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                          use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                          include.y = T, varname.y = "expats_per_100k", lagTerms.y = 1,
                          fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                          varname.reg.fur = c("travel_rest_ind", "W", 
                                              "deaths_per_100k.o", "deaths_per_100k.d", 
                                              "cases_per_100k.o", "cases_per_100k.d", 
                                              "mobility_mean.o", "mobility_mean.d"), 
                          lagTerms.reg.fur = c(0, 0, 1, 1, 0, 0, 0, 0), 
                          w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                          opt.meth = "none")
summary(altlag_model)
mtest.fct(altlag_model)
jtest.fct(altlag_model)
wald.fct(altlag_model, param = "all")

##
summary(simple_model)
summary(dummy_model)
summary(exmort_model)
summary(exmortlag_model)
summary(string_model)
summary(og_model)
summary(oglag_model)

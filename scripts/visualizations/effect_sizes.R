#### Effect sizes plots ####
#### Plot estimate effect sizes w/ 95% confidence intervals for origin-destination country pairs
### Effect sizes from- simple model (no covariates), model w/ covariates = W, excess mortality, stringency index

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
library(plm)
library(lmtest)
library(pdynmc)

### Load data
setwd("data/clean_data")
Data <- read_csv("full_unadj.csv") %>% 
  # Create variable for expats (not log transformed)
  mutate(expats = exp(log_expats)) 

#### Run models to get effect size = log(expats{t=1})-log(expats{t=0}) ####
#### Simple- no covariates
simple_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                       use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                       include.y = T, varname.y = "log_expats", lagTerms.y = 1,
                       fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                       varname.reg.fur = "travel_rest_ind", lagTerms.reg.fur = c(0), 
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "none")

#### Model w/ covariates
# Original model (no lag)
cov_model <- pdynmc(dat = Data, varname.i = "o_d_pair", varname.t = "date", 
                   use.mc.diff = T, use.mc.lev = F, use.mc.nonlin = F, 
                   include.y = T, varname.y = "log_expats", lagTerms.y = 1,
                   fur.con = T, fur.con.diff = T, fur.con.lev = F, 
                   varname.reg.fur = c("travel_rest_ind", "W", 
                                       "excess_mortality_per_100k.o", "excess_mortality_per_100k.d", 
                                       "string_mean.o", "string_mean.d"), 
                   lagTerms.reg.fur = rep(0, 6), 
                   w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                   opt.meth = "none")

#### Data preparation ####
#### Get pre-pandemic expats from each origin-destination pair
Expats_bline <- filter(Data, date == ymd("2020-02-16")) %>% 
  select(origin, country, o_d_pair, od_id, log_expats)

#### Write function to calculate effect sizes
effect_size <- function(data, model, model_type) {
  # Get coefficient & variance of treatment term
  coefficient = model$coefficients[2]
  variance = vcov(model)[2, 2]
  
  # Get treatment effect point estimate
  baseline <- data$log_expats
  log_expats_treated = baseline+coefficient
  log_expats_untreated = baseline
  expats_treated = exp(log_expats_treated)
  expats_untreated = exp(log_expats_untreated)
  
  treatment_effect = expats_treated-expats_untreated
  
  ## Calculate standard error of treatment effect using delta method
  # 1st derivative of expats treated
  expats_treated_1d <- exp(log_expats_treated)
  # Standard error
  se <- sqrt(expats_treated_1d^2*variance)
  
  ## Upper & lower bounds
  effect_lo <- expats_treated+qnorm(.025)*se-expats_untreated
  effect_hi <- expats_treated+qnorm(.975)*se-expats_untreated
  
  ## Return results
  results <- tibble(treatment_effect, effect_lo, effect_hi) %>% 
    rename_with(~ paste(., model_type, sep = "_"))
  return(results)
}

#### Calculate effect sizes & put them back in data
### Simple model
Expats_effect <- Expats_bline %>% 
  bind_cols(effect_size(data = Expats_bline, model = simple_model, model_type = "simple")) %>%
  ### Covariates model
  bind_cols(effect_size(data = Expats_bline, model = cov_model, "covs"))

#### Plotting ####
#### Simple model plot
ggplot(Expats_effect, aes(x = country, y = treatment_effect_simple, color = origin)) + 
  geom_pointrange(position = position_dodge(width = .5), aes(ymax = effect_hi_simple, ymin = effect_lo_simple)) + 
  geom_hline(yintercept = 0) + coord_flip() + scale_x_discrete(limits = rev, name = "Destination Country") + 
  scale_y_continuous(trans = pseudo_log_trans(), 
                     breaks = c(-100000, -10000, -1000, -100, -10, 0), limits = c(-82500, 0), 
                     labels = scales::comma, name = "Travel Restriction Effect Size") + 
  scale_color_discrete(name = "Origin Country") + 
  theme(axis.title = element_text(face="bold"))
### Export plot
setwd(here::here())
setwd("figures")
ggsave("simple_model_effect_size.png", width = 500, height = 500, units = "px", scale = 6)

#### Model w/ covariates plot
ggplot(Expats_effect, aes(x = country, y = treatment_effect_covs, color = origin)) + 
  geom_pointrange(position = position_dodge(width = .5), aes(ymax = effect_hi_covs, ymin = effect_lo_covs)) + 
  geom_hline(yintercept = 0) + coord_flip() + scale_x_discrete(limits = rev, name = "Destination Country") + 
  scale_y_continuous(trans = pseudo_log_trans(), 
                     breaks = c(0, 10, 100, 1000, 10000, 100000), limits = c(0, 82500), 
                     labels = scales::comma, name = "Travel Restriction Effect Size") + 
  scale_color_discrete(name = "Origin Country") + 
  theme(axis.title = element_text(face="bold"))
### Export plot
ggsave("covariates_model_effect_size.png", width = 500, height = 500, units = "px", scale = 6)

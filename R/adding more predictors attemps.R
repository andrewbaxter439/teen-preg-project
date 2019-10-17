library(readxl)
library(tidyverse)
library(stringr)
library(broom)
library(magrittr)
library(Synth)
library(Rcpp)
library(purrr)
library(svglite)
library(SPHSUgraphs)
library(plotly)
library(knitr)
`-.gg` <- function(e1, e2) e2(e1)
load('Data/synth_data.rdata') # outputted from 'Synth_data.R'
load('Data/synth_data_b.rdata') # outputted from 'Synth_data.R'
load("Data/filtered_itsp.rdata")  # outputted from 'Synth_create_sps.R'
load("Data/placebo_country_b.rdata")  # outputted from 'Synth_create_sps.R'
load("Data/time_placebos_b.rdata")  # outputted from 'Synth_time_pbs.R'
source('R/Synth_functions.R')

# Predictor 3 - spending on education ------------------------------------------------

edu_all <- read_csv("Downloaded data files/education_spending.csv", skip = 3)
edu_all[edu_all$"Country Code" == "USA",]$`Country Name` <- "United States of America"

edu_pred <- edu_all %>% filter(`Country Name` %in% c(u_18_ccodes_f$Country, "United Kingdom")) %>% 
  select(Country = `Country Name`, `1990`:`2013`) %>% 
  gather("Year", "edu_spend", -1) %>% 
  mutate(yrgrp = ifelse(Year<1996, 1, ifelse(Year<1999, 2, 3))) %>% 
  group_by(yrgrp, Country) %>% 
  mutate(edu_spend_mean = mean(edu_spend, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Country = ifelse(Country=="United Kingdom", "England and Wales", Country),
         Year = as.numeric(Year)) %>% 
  select(Country, Year, edu_spend_mean)

newdata <- synthData_u20_filt %>% left_join(edu_pred, by = c("Country", "Year")) %>% filter(Country!="Scotland", Country != "United States of America")

sp_u20_edu <- list(
  list("edu_spend_mean", 1995, "mean"),
  list("edu_spend_mean", 1997, "mean")
)

newdata %>% 
  filter(Year < 1999) %>% 
  ggplot(aes(Year, edu_spend_mean, col = Country)) +
  geom_line() +
  geom_line(data = newdata %>% filter(Country == "England and Wales", Year < 1999), size = 2)

cheeky_data <- newdata %>% 
  mutate(pRate = ifelse(Country == "England and Wales", pRate, pRate*2.5))

opt_y1 <- 1990
synthPrep(data.frame(cheeky_data[,c(1:4, 6, 10)]),
          "trial_synth2",
          dependent = "pRate",
          special.predictors = append(sp_u20_filt, sp_u20_edu), 
          time.optimise.ssr = opt_y1:1998,
          time.predictors.prior = opt_y1:1998,
          time.plot = opt_y1:2013)


gg_synth(md = md_trial_synth2) +
  scale_linetype_manual(name = "Data", values = c("Synthetic" = "solid", "Treated" = "solid"))
st_trial_synth2$tab.v
st_trial_synth2$tab.w

# playing around ----------------------------------------------------------


as.matrix(dp_u20_gdp$Z0) %*% st_u20_gdp$tab.w$w.weights

md_u20_gdp %>% filter(Year<1999, Group == "Synthetic")

V <- diag(as.numeric(st_u20_gdp$tab.v), length(st_u20_gdp$tab.v), length(st_u20_gdp$tab.v))
V2 <- diag(c(0.1,0.1,0.3,0.1,0.1,0.3), 6, 6)
W <- st_u20_gdp$tab.w$w.weights

X1 <- dp_u20_gdp$X1
X0 <- dp_u20_gdp$X0
Z1 <- dp_u20_gdp$Z1
Z0 <- dp_u20_gdp$Z0

(t(X0) %*% V) %*% X0

t(X1 - X0 %*% W) %*% V2 %*% (X1 - X0 %*% W)

t(Z1 - Z0 %*% W) %*% (Z1 - Z0 %*% W)


so_u20_gdp$loss.w
so_u20_gdp$loss.v

pre_MSPE(md_u20_gdp)


# predictor 4 - mobile phone ownership ------------------------------------

sd_noScot <- synthData_u20_filt %>% filter(Country != "Scotland")
cc_noScot <- u_20_ccodes_f %>% filter(Country != "Scotland")


it_u20_mob <- testSynthIterations(
  yrs = 1990:2013,
  pred = "MobilePhones",
  data = sd_noScot,
  ccodes = cc_noScot,
  n = 2,
  predictors = NULL,
  time.optimise = 1990:1998,
  dependent = "pRate"
) %>%
  arrange(groups, mspe)



# load data ---------------------------------------------------------------

load("Data/sp_iterations.rdata")

it_u20_mob %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

sp_u20_mob <- it_u20_mob$sPred[it_u20_mob['iteration']==1522][1]
it_u20_mob$v_weights[it_u20_mob['iteration']==1522][[1]]$v_weight

synthPrep(sd_noScot,
          "u20_mobiles",
          dependent = "pRate",
          )
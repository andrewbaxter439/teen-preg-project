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

# Predictor 3 - spending on education  -------------------------------------

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


# ** other edu data from oecd ---------------------------------------------

edu_full <- read_csv("Downloaded data files/ed_spend_oecd.csv") %>% mutate(Code = LOCATION)
ccodes <- read_tsv("country_codes.txt") %>% 
  bind_rows(tibble(Code = c("GBR", "GBR", "FRA", "DEU"),
                   Country = c("England and Wales", "Scotland", "France", "Germany")))

edu_spend <- edu_full %>% 
  left_join(ccodes, by = "Code") %>% 
  select(Country, Year = TIME, edu_spend = Value)

newdata <- synthData_u20_filt %>% 
  left_join(edu_spend, by = c("Country", "Year")) %>% 
  filter(Country != "Scotland")


# ** join with other data -------------------------------------------------



# newdata <- synthData_u20_filt %>% left_join(edu_pred, by = c("Country", "Year")) %>% filter(Country!="Scotland")

sp_u20_edu <- list(
  list("edu_spend", 1995:1998, "mean"),
  list("edu_spend", 1999:2007, "mean"),
  list("edu_spend", 2008:2013, "mean")
)

newdata %>% 
  filter(Year < 1999) %>% 
  ggplot(aes(Year, edu_spend, col = Country)) +
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

# predictor 4 - mobile phone ownership ------------------------------------

sd_noScot <- newdata %>% 
  filter(Country == "Iceland") %>% 
  select(-edu_spend) %>% 
  left_join(edu_pred %>% filter(Country == "Iceland") %>% select(Country, Year, edu_spend = edu_spend_mean), by = c("Country", "Year")) %>% 
  bind_rows(newdata %>% filter(Country != "Iceland")) %>% 
  select(-rate) %>% 
  left_join(synthData_u18_filt %>% select(Country, Year, rate) %>% filter(Country!="Scotland"))

# Iterating bits ----------------------------------------------------------



it_u20_mob <- testSynthIterations(
  yrs = 1990:2013,
  pred = "MobilePhones",
  data = sd_noScot,
  ccodes = cc_noScot,
  n = 4,
  predictors = NULL,
  time.optimise = 1990:1998,
  dependent = "pRate"
) %>%
  arrange(groups, mspe)

it_u20_urb <- testSynthIterations(
  yrs = 1990:2013,
  pred = "UrbanPop",
  data = sd_noScot,
  ccodes = cc_noScot,
  n = 4,
  predictors = NULL,
  time.optimise = 1990:1998,
  dependent = "pRate"
) %>%
  arrange(groups, mspe)

it_u20_gdp <- testSynthIterations(
  yrs = 1990:2013,
  pred = "GDPperCap",
  data = sd_noScot,
  ccodes = cc_noScot,
  n = 4,
  predictors = NULL,
  time.optimise = 1990:1998,
  dependent = "pRate"
) %>%
  arrange(groups, mspe)

save(it_u20_mob, it_u20_gdp, it_u20_urb, file = "Data/sp_iterations.rdata")

# load data ---------------------------------------------------------------

load("Data/sp_iterations.rdata")

it_u20_mob %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u20_gdp %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u20_urb %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

# Checking for unweighted parts
it_u20_mob$v_weights[it_u20_mob['iteration']==1522][[1]]$v_weight
it_u20_gdp$v_weights[it_u20_gdp['iteration']==171][[1]]$v_weight
it_u20_urb$v_weights[it_u20_urb['iteration']==2048][[1]]$v_weight

# Selecting for weighted years in each special predictor
# sp_u20_mob <- it_u20_mob$sPred[it_u20_mob['iteration']==1522][1][[1]][1:2]
# sp_u20_gdp <- it_u20_gdp$sPred[it_u20_gdp['iteration']==171][1][[1]][2:4]
# sp_u20_urb <- it_u20_urb$sPred[it_u20_urb['iteration']==2048][1][[1]][1]

sp_u20_mob <- it_u20_mob$sPred[it_u20_mob['iteration']==1522][1][[1]]
sp_u20_gdp <- it_u20_gdp$sPred[it_u20_gdp['iteration']==171][1][[1]]
sp_u20_urb <- it_u20_urb$sPred[it_u20_urb['iteration']==2048][1][[1]]

save(
  sp_u20_edu,
  sp_u20_mob,
  sp_u20_gdp,
  sp_u20_urb,
  file = "Data/other_predictors.rdata"
)

# under 20 - all predictors -----------------------------------------------
load("Data/other_predictors.rdata")

synthPrep(sd_noScot,
          "u20_all",
          dependent = "pRate",
          # special.predictors = sp_u20_filt,
          special.predictors = append(sp_u20_filt, sp_u20_mob) %>% append(sp_u20_gdp) %>%
            append(sp_u20_urb) %>%
            append(sp_u20_edu),
          time.optimise.ssr = 1996:1998,
          time.plot = 1990:2013,
          time.predictors.prior = 1990:1998
)

gg_synth(md = md_u20_all) + xlim(1990, 2013)
st_u20_all$tab.w
st_u20_all$tab.v


# Under-18s - all predictors ----------------------------------------------

synthPrep(sd_noScot,
          "u18_all",
          dependent = "rate",
          special.predictors = append(sp_u20_gdp, sp_u20_mob) %>% 
            append(sp_u20_urb) %>% 
            append(sp_u18_filt) %>% 
            append(sp_u20_edu),
          time.optimise.ssr = 1996:1998,
          time.plot = 1990:2013,
          time.predictors.prior = 1990:1998
)

gg_synth(md = md_u18_all) + xlim(1990, 2013)
st_u18_all$tab.w
st_u18_all$tab.v

ccodes_ns <- sd_noScot %>% filter(Country != "Scotland") %>% 
  select(Country, Code) %>% 
  unique() %>% 
  arrange(Code)

# save(sd_noScot, ccodes_ns, file = "Data/noScot.rdata")

beta <- -0.13142
serr <- 0.0882116
tval <- beta/serr

2*pnorm(-abs(tval))

2*pt(-abs(tval), df = 41)



tnew <- (-0.304-beta)/serr
2*pt(-abs(tnew), df = 42, lower.tail = TRUE)


library(tidyverse)
library(readxl)
library(broom)
library(nlme)
library(car)


# all.UK.rates - Import conception rates for GBR countries -------------------------------------------

all.UK.rates <-
  read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 18")

load("Data/all_uk_rates.rdata")

# Create datasets --------------------------------------------------------------------------------------------

#** EngData - England only -------------------------------------------------------------------------------------

EngData <- 
  all_UK_rates[['Under 18']] %>% 
  filter(Country == "England") %>% 
  mutate(
    Cat1 = ifelse(Year < 1999, 0, 1),
    Cat2 = ifelse(Year <= 2007, 0, 1),
    Year = as.numeric(Year),
    Time = 1:nrow(.),
    Trend1 = ifelse(Cat1 == 0, 0, 1:nrow(.) - nrow(filter(., Cat1 ==
                                                            0))),
    Trend2 = ifelse(Cat2 == 0, 0, 1:nrow(.) - nrow(filter(., Cat2 ==
                                                            0))),
    PillScare = ifelse(Year > 1995, 1, 0)
  )

#** EngScotContro data setup - Eng v Scot -----------------------------------------------

EngScotContro <-
  all_UK_rates[['Under 18']] %>% 
  filter(Country %in% c("England", "Scotland")) %>% 
  filter(Year > 1991,!is.na(Value)) %>%
  arrange(Country) %>%
  mutate(
    England = ifelse(Country == "England", 1, 0),
    Year = as.numeric(Year),
    Time = Year - min(Year) + 1,
    Cat1 = ifelse(Year < 1999, 0, 1),
    Cat2 = ifelse(Year <= 2007, 0, 1),
    Trend1 = ifelse(Cat1 == 0, 0, Year - 1998),
    Trend2 = ifelse(Cat2 == 0, 0, Year - 2007),
    PillScare = ifelse(Year > 1995, 1, 0)
  ) %>%
  mutate_at(., colnames(.)[5:9], list(Eng = ~ .*England))  # Potentially not necessary

#** EngWalContro data setup - Eng vs Wales as control ----------------------------------------------------------

EngWalContro <-
  all_UK_rates[['Under 18']] %>% 
  filter(Country %in% c("England", "Wales")) %>% 
  filter(Year > 1991,!is.na(Value)) %>%
  arrange(Country) %>%
  mutate(
    England = ifelse(Country == "England", 1, 0),
    Year = as.numeric(Year),
    Time = Year - min(Year) + 1,
    Cat1 = ifelse(Year < 1999, 0, 1),
    Cat2 = ifelse(Year <= 2007, 0, 1),
    Trend1 = ifelse(Cat1 == 0, 0, Year - 1998),
    Trend2 = ifelse(Cat2 == 0, 0, Year - 2007),
    PillScare = ifelse(Year > 1995, 1, 0)
  ) %>%
  mutate_at(., colnames(.)[5:9], list(Eng = ~ .*England))  # Potentially not necessary


# engscot with pill scare and shock ---------------------------------------

mod_esps <- gls(
  Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare,
  data = EngScotContro,
  correlation = corARMA(
    p = 1,
    q = 0,
    form = ~ Time | England
  ),
  method = "ML"
)

summary(mod_esps)

linearHypothesis(mod_esps, "Trend1_Eng = -0.4", test = "Chisq")

2*pt(-abs((beta)/serr), df = 41)
pt(abs((-0.4-beta)/serr), df = 41, lower.tail = FALSE)

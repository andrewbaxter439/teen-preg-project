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


# models to test ----------------------------------------------------------

modEng <- gls(Value ~ Time + Cat1 + Trend1,
              data = EngData,
              method = "ML",
              correlation = corARMA(p = 1, form =  ~ Time))

modEng_pill <- gls(Value ~ Time + Cat1 + Trend1 + PillScare,
                   data = EngData,
                   method = "ML",
                   correlation = corARMA(p = 1, form =  ~ Time))

summary(modEng)
summary(modEng_pill)
anova(modEng, modEng_pill)


EngData %>% 
  mutate(pred_pill = predict(modEng_pill),
         pred_nopill = predict(modEng),
         spe_pill = (Value - pred_pill)^2,
         spe_nopill = (Value - pred_nopill)^2
         ) %>% 
  filter(Year < 1999) %>% 
  summarise(across(starts_with("spe"), mean, .names = "mspe_{str_extract(.col, '[^_]*$')}"),
            rsq_nopill = cor(Value, pred_nopill)^2,
            rsq_pill = cor(Value, pred_pill)^2) %>% 
  pivot_longer(everything(), names_to = c("var", "group"), values_to = "est", names_sep = "_") %>% 
  pivot_wider(var, names_from = "group", values_from = "est")


lm(Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
   data = EngScotContro) %>%
  summary() %>% 
  {  print(ggplot(eval(.$call$data), aes(
    Year, Value, group = interaction(Country, Cat1), col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
  }

modScot99_p1 <-
  gls(
    Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
    data = EngScotContro,
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )
summary(modScot99_p1)
coef(modScot99_p1)

modScot99_p1_pill <- 
  gls(
    Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng +PillScare,
    data = EngScotContro,
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )

summary(modScot99_p1_pill)
coef(modScot99_p1_pill)

anova(modScot99_p1, modScot99_p1_pill)  

# models with pill scare and pre-intervention only ------------------------

pre_int_dat <- filter(EngData, Year<1999)

modEng <- gls(Value ~ Time,
              data = pre_int_dat,
              method = "ML",
              correlation = corARMA(p = 1, form =  ~ Time))

modEng_pill <- gls(Value ~ Time + PillScare,
                   data = pre_int_dat,
                   method = "ML",
                   correlation = corARMA(p = 1, form =  ~ Time))

summary(modEng)
summary(modEng_pill)
anova(modEng, modEng_pill)


cor(pre_int_dat$Value, predict(modEng))^2
cor(pre_int_dat$Value, predict(modEng_pill))^2

mean((pre_int_dat$Value - predict(modEng))^2)
mean((pre_int_dat$Value - predict(modEng_pill))^2)


# parallel pre-intervention trends ----------------------------------------


modScot99_p1_parr <- 
  gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + PillScare,
    data = EngScotContro,
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )

summary(modScot99_p1_parr)
coef(modScot99_p1_parr)

anova(modScot99_p1_pill, modScot99_p1_parr)

modScot99_p1_t2 <- 
  gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare,
    data = EngScotContro,
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )

summary(modScot99_p1_t2)
coef(modScot99_p1_t2)

anova(modScot99_p1_parr, modScot99_p1_t2)


# parallel - pre intervention data only -----------------------------------

pre_EngScot <- filter(EngScotContro, Year < 1999)

modScot99_p1_pill <- 
  gls(
    Value ~ Time + England + Time_Eng + PillScare,
    data = pre_EngScot,
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )

modScot99_p1_parr <- 
  gls(
    Value ~ Time + England + PillScare,
    data = pre_EngScot,
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )

summary(modScot99_p1_parr)
coef(modScot99_p1_parr)

cor(pre_EngScot$Value, predict(modScot99_p1_pill))^2
cor(pre_EngScot$Value, predict(modScot99_p1_parr))^2

mean((pre_EngScot$Value - predict(modScot99_p1_pill))^2)
mean((pre_EngScot$Value - predict(modScot99_p1_parr))^2)


anova(modScot99_p1_pill, modScot99_p1_parr)


# testing multiple common shock dates -------------------------------------

modScot99_t2008 <- 
  gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare,
    data = EngScotContro,
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )

modScot99_t2009 <- 
  gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare,
    data = EngScotContro %>% mutate(Trend2 = ifelse(Year > 2008, Year - 2008, 0)),
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )

modScot99_t2007 <- 
  gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare,
    data = EngScotContro %>% mutate(Trend2 = ifelse(Year > 2006, Year - 2006, 0)),
    correlation = corARMA(p = 1, form =  ~ Time | England),
    method = "ML"
  )

anova(modScot99_t2008, modScot99_t2009, modScot99_t2007)

library(tidyverse)
library(readxl)
library(broom)
library(nlme)
library(car)

testAutocorr <- function(model, data=NULL, max.lag = 10, time.points = 25) {
  data <- eval(model$call$data)  # Only works if 'lm' call has dataframe named in bracket
  print(dwt(model, max.lag = max.lag, alternative = "two.sided"))
  par(cex = 0.7, mai = c(0.1, 0.1, 0.2, 0.1))
  par(fig = c(0.03, 1, 0.8, 1))
  plot(
    data$Time[1:time.points],
    residuals(model)[1:time.points],
    type = 'o',
    pch = 16,
    col = "red"
  )
  
  par(fig = c(0.03, 0.5, 0.05, 0.75), new = TRUE)
  acf(residuals(model))
  
  par(fig = c(0.55, 1, 0.05, 0.75), new = TRUE)
  acf(residuals(model), type = 'partial')
}

constructCIRibbon <- function(newdata, model) {
  newdata <- newdata %>%
    mutate(Value = predict(model, newdata = newdata))
  mm <- model.matrix(as.formula(paste0("~ ", model$call$model[3])),
                     data = newdata)
  vars <- mm %*% vcov(model) %*% t(mm)
  sds <- sqrt(diag(vars))
  newdata <- newdata %>% mutate(lowCI = Value - 1.96 * sds,
                                HiCI = Value + 1.96 * sds)
}

# Import Scotland data ---------------------------------------------------------------------------------------

Scotland.conc <-
  read_xls(
    "Downloaded data files/ScotISD2018 report.xls",
    sheet = "Table1",
    range = "B33:P55",
    col_names = FALSE
  ) %>%
  select(
    Year = '..1',
    '<16' = '..11',
    '<18' = '..13',
    '<20' = '..15'
  )

# Scotland births to estimate pregnancies pre-1994
# Data from NRS

Scot.births <-
  read_xlsx(
    "Downloaded data files/Scotland births.xlsx",
    range = "AQ4:BW16",
    col_names = TRUE
  )[c(3:10, 12), ] %>%
  select(Age = '..33', 1:32) %>%
  mutate_all(as.numeric)

# Create age calculation groups ### NOW DEFUNCT

birthAgeGrps <- tibble(Age = factor(c(12:15, 12:17, 12:19)),
                       agegrp = factor(
                         c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3),
                         labels = c("Under 16", "Under 18", "Under 20")
                       ))  # Three ranges of births

sumScotBirths <- Scot.births %>%
  gather("Year", "Total",-1) %>%
  mutate(Year = as.numeric(Year)) %>%
  merge(birthAgeGrps) %>%
  group_by(Year, agegrp) %>%
  summarise(sumBirths = sum(Total)) # create summary

# merge with scotland births and population

Scot.birth.rates <- Scot.births %>%
  gather("Year", "Total",-1) %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(allpops %>%
              filter(Code == "GBR_SCO") %>%
              select(Age, Female, Year)) %>%
  merge(agecalcs) %>%
  group_by(Year, agegrp, agecat) %>%
  summarise(popsum = sum(Female), birthsum = sum(Total)) %>%
  ungroup() %>%
  filter(agegrp == agecat) %>%
  mutate(agecat = factor(agecat,
                         labels = c("Under 16", "Under 18", "Under 20"))) %>%
  mutate(rate = 1000 * birthsum / popsum)



# all.UK.rates - Import conception rates for GBR countries -------------------------------------------

all.UK.rates <-
  read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 18")

all.UK.rates %>% filter(Country == "Scotland" |
                          Country == "England" |
                          Country == "Wales") %>%
  gather("Year", "Value",-1) %>%
  mutate(Category = ifelse(Year < 1999, 0,
                           ifelse(Year < 2007, 1, 2)),
         Year = as.numeric(Year)) %>%
  group_by(Country, Category) %T>%
  {
    print(group_map(., ~ tidy(lm(Value ~ Year, data = .))) %>%
            arrange(Category, term, Country))
  } %>%
  {
    ggplot(., aes(
      x = Year,
      y = Value,
      group = interaction(Category, Country),
      col = Country
    )) +
      geom_point() +
      geom_smooth(method = "lm")
  }


# EngMod - England data in ITS with break at 1999 ------------------------------------------------------------

EngData <- all.UK.rates %>% filter(Country == "England") %>%
  select(Country, '1992':'2016') %>%
  gather("Year", "Value",-1) %>%
  mutate(
    Cat1 = ifelse(Year < 1999, 0, 1),
    Cat2 = ifelse(Year <= 2007, 0, 1),
    Year = as.numeric(Year),
    Time = 1:nrow(.),
    Trend1 = ifelse(Cat1 == 0, 0, 1:nrow(.) - nrow(filter(., Cat1 ==
                                                            0))),
    Trend2 = ifelse(Cat2 == 0, 0, 1:nrow(.) - nrow(filter(., Cat2 ==
                                                            0)))
  )

EngData %>% ggplot(aes(x = Year, y = Value, group = Cat1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

EngMod <- lm(Value ~ Time + Cat1 + Trend1, data = EngData)
summary(EngMod)

# Test for autocorrelation

testAutocorr(EngData,EngMod)


# EngMod2 - proposing new effect at Y=2007 -------------------------------------------------------------------

EngData %>% ggplot(aes(
  x = Year,
  y = Value,
  group = interaction(Cat1, Cat2)
)) +
  geom_point() +
  geom_smooth(method = "lm")

EngMod2 <-
  lm(Value ~ Time + Cat1 + Trend1 + Cat2 + Trend2, data = EngData)
summary(EngMod2)

## Test for autocorrelation

testAutocorr(EngData, EngMod2)  # Potential MA3?

EngMod2_q3 <- gls(
  Value ~ Time + Cat1 + Trend1 + Cat2 + Trend2,
  data = EngData,
  correlation = corARMA(q = 3, form =  ~ Time),
  method = "ML"
)
summary(EngMod2_q3)
coef(EngMod2_q3)

EngMod2_q3 %>% update(correlation = corARMA(q = 4, form =  ~ Time)) %>%
  anova(EngMod2_q3)

EngMod2_q3 %>% update(correlation = NULL) %>%
  anova(EngMod2_q3)

EngMod2_q3 %>% update(correlation = corARMA(q = 3, p = 1, form =  ~ Time)) %>%
  anova(EngMod2_q3)  # Significantly different

EM2q3_co <- EngMod2_q3$coefficients

EngMod2_q3_cfac <- tibble(
  Time = 8:25,
  Cat = c(rep.int(0, 9), rep(1, 9)),
  Trend1 = c(1:18),
  Trend2 = c(rep(0, 9), c(1:9)),
  Value = (
    EM2q3_co[1] +
      EM2q3_co[2] * Time +
      EM2q3_co[3] * Cat +
      EM2q3_co[4] * Trend1 * Cat
  )
)

EngData %>%
  mutate(predict = predict(EngMod2_q3)) %>%
  ggplot(aes(Time, Value, group = interaction(Cat1, Cat2))) +
  geom_point() +
  geom_line(aes(y = predict), col = "red") +
  geom_line(
    data = EngMod2_q3_cfac,
    aes(x = Time, y = Value, group = Cat),
    linetype = "dashed",
    col = "#FC8D62",
    size = 1,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  geom_vline(xintercept = 16.5,
             linetype = "dotted",
             col = "#000000CC")

# EngScotContro data setup - Eng v Scot -----------------------------------------------

EngScotContro <-
  all.UK.rates %>% filter(Country == "Scotland" |
                            Country == "England") %>%
  gather("Year", "Value",-1) %>%
  filter(Year > 1991,!is.na(Value)) %>%
  arrange(Country) %>%
  mutate(
    England = ifelse(Country == "England", 1, 0),
    Year = as.numeric(Year),
    Time = Year - min(Year) + 1,
    Cat1 = ifelse(Year < 1999, 0, 1),
    Cat2 = ifelse(Year <= 2007, 0, 1),
    Trend1 = ifelse(Cat1 == 0, 0, Year - 1998),
    Trend2 = ifelse(Cat2 == 0, 0, Year - 2007)
  ) %>%
  mutate_at(., colnames(.)[5:9], list(Eng = ~ ._Eng))  # Potentially not necessary

## modScot99 - Comparing for pre-post 1999 --------------------------------------------

EngScotContro %T>%
{
  print(ggplot(., aes(
    Year, Value, group = interaction(Country, Cat1), col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
} %>%
  lm(Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
     data = .) %>%
  assign("modScot99", ., envir = .GlobalEnv) %>%
  print(.$coefs)
#  summary()

testAutocorr(EngScotContro, modScot99)  # Decay in ACF, sig at lag=3 in PACF. Indicates AR3

modScot99_p3 <-
  gls(
    Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
    data = EngScotContro,
    corARMA(p = 3, form =  ~ Time | England),
    method = "ML"
  )
summary(modScot99_p3)
coef(modScot99_p3)

## Testing other models
modScot99_p3 %>% update(correlation = corARMA(p = 4, form = ~ Time |
                                                England)) %>%
  anova(modScot99_p3)

modScot99_p3 %>% update(correlation = NULL) %>%
  anova(modScot99_p3)

modScot99_p3 %>% update(correlation = corARMA(
  p = 3,
  q = 1,
  form = ~ Time | England
)) %>%
  anova(modScot99_p3)  # Conclusion: p3 is a bad model!

modScot99_p3_cfac <- tibble(
  Time = c(8:25),
  Trend1 = c(1:18),
  Value = (
    modScot99_p3$coefficients[1] +
      modScot99_p3$coefficients[2] * Time +
      modScot99_p3$coefficients[3] +
      modScot99_p3$coefficients[4] * Time +
      modScot99_p3$coefficients[5] +
      modScot99_p3$coefficients[6] * Trend1
  )
)

EngScotContro %>%
  mutate(predict = predict(modScot99_p3)) %>%
  ggplot(aes(Time, Value, col = Country, group = interaction(Country, Cat1))) +
  geom_point() +
  geom_line(
    data = modScot99_p3_cfac,
    aes(x = Time, y = Value),
    linetype = "dashed",
    col = "#FC8D62",
    size = 1,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = predict), size = 1) +
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC")

## modScot07 - Comparing for pre-post 2007 --------------

EngScotContro %T>%
{
  print(ggplot(., aes(
    Year, Value, group = interaction(Country, Cat2), col = Country
  )) +
    geom_point() + geom_smooth(method = "lm"))
} %>%
  lm(Value ~ Time + England + Time_Eng + Cat2 + Trend2 + Cat2_Eng + Trend2_Eng,
     data = .) %>%
  assign("modScot07", ., envir = .GlobalEnv) %>%
  summary()

## modScot99_07 - Comparing for thee stages, split at 1999 and 2007 --------------------

EngScotContro %T>%
{
  print(ggplot(., aes(
    Year,
    Value,
    group = interaction(Country, Cat1, Cat2),
    col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
} %>%
  lm(
    Value ~ Time +
      England +
      Time_Eng +
      Cat1 +
      Trend1 +
      Cat1_Eng +
      Trend1_Eng +
      Cat2 +
      Trend2 +
      Cat2_Eng +
      Trend2_Eng,
    data = .
  ) %>%
  assign("modScot99_07", ., envir = .GlobalEnv) %>%
  summary()

## test autocorrelation
testAutocorr(EngScotContro, modScot99_07)  # Assume NULL for now

modScot99_07_null <- gls(
  Value ~ Time +
    England +
    Time_Eng +
    Cat1 +
    Trend1 +
    Cat1_Eng +
    Trend1_Eng +
    Cat2 +
    Trend2 +
    Cat2_Eng +
    Trend2_Eng,
  data = EngScotContro,
  correlation = NULL,
  method = "ML"
)

summary(modScot99_07_null)

modScot99_07_null_cfac <- EngScotContro[8:25, ] %>%
  select(Time, England, Cat1, Cat2, Trend1, Trend2, Time_Eng) %>%
  mutate(
    Cat1_Eng = c(rep(0, 9), rep(1, 9)),
    Trend1_Eng = c(rep(0, 9), 10:18),
    Cat2_Eng = 0,
    Trend2_Eng = 0
  )

modScot99_07_null_cfac <-
  modScot99_07_null_cfac %>%
  left_join(constructCIRibbon(modScot99_07_null_cfac, modScot99_07_null))

## Graphing final model

EngScotContro %>%
  mutate(predict = predict(modScot99_07_null)) %>%
  ggplot(aes(
    Time,
    Value,
    col = Country,
    group = interaction(Country, Cat1, Cat2)
  )) +
  geom_point() +
  geom_smooth(
    # predicted values and 95% Confidence Interval
    data = modScot99_07_null_cfac,
    aes(
      x = Time,
      y = Value,
      ymin = lowCI,
      ymax = HiCI,
      group = Cat2
    ),
    stat = 'identity',
    linetype = "longdash",
    col = "#FC8D62",
    fill = "#FC8D62",
    size = 1,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = predict, linetype = Country), size = 1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  geom_vline(xintercept = 16.5,
             linetype = "dotted",
             col = "#000000CC") +
  theme(panel.background = element_blank()) +
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(expand = c(0, 0))

# modScotPI - Comparing pre-1999 and post-2007, proposing phase-in -------------------------------------------

lm(Value ~ Time + England + Time_Eng + Cat2 + Trend2 + Cat2_Eng + Trend2_Eng,
   data = {EngScotContro %>% filter(Year < 1999 | Year > 2007)}) %>%
  assign("modScotPI", ., envir = .GlobalEnv) %T>%
  {print(ggplot(data = eval(.$call$data), aes(
    Year,
    Value,
    group = interaction(Country, Cat1, Cat2),
    col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))} %>% 
  summary()

testAutocorr(modScotPI, max.lag=14, time.points = 16)  # Possibly no autocorrelation? Sig at PACF 12 and 13

modScotPI_null <-
  gls(
    Value ~ Time + England + Time_Eng + Cat2 + Trend2 + Cat2_Eng + Trend2_Eng,
    data = EngScotContro %>% filter(Year < 1999 | Year > 2007),
    correlation = NULL,
    method = "ML"
  )

summary(modScotPI_null)
confint(modScotPI_null)

modScotPI_null_cfac <- tibble(
  Time = c(17:25),
  England = 1,
  Cat2 = 1,
  Trend2 = c(1:9),
  Time_Eng = c(17:25),
  Cat2_Eng = 0,
  Trend2_Eng = 0
)  # Emlulating England with no added effect

modScotPI_null_cfac <-
  modScotPI_null_cfac %>%
  left_join(constructCIRibbon(modScotPI_null_cfac, modScotPI_null))

EngScotContro %>%
  filter(Year < 1999 | Year > 2007) %>%
  mutate(predict = predict(modScotPI_null)) %>%
  ggplot(aes(Time, Value, col = Country, group = interaction(Country, Cat2))) +
  geom_point(data = EngScotContro) +
  geom_smooth(
    # predicted values and 95% Confidence Interval
    data = modScotPI_null_cfac,
    aes(
      x = Time,
      y = Value,
      ymin = lowCI,
      ymax = HiCI
    ),
    stat = 'identity',
    linetype = "dashed",
    col = "#FC8D62",
    fill = "#FC8D62",
    size = 1,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = predict), size = 1) +
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  theme(panel.background = element_blank()) +
  geom_rect(
    xmin = 7.5,
    xmax = 16.5,
    ymin = 0,
    ymax = 60,
    fill = "grey",
    alpha = 0.01,
    inherit.aes = FALSE
  ) +
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(expand = c(0, 0))

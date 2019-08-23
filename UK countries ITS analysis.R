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
    mutate(Predict = predict(model, newdata = newdata))
  mm <- model.matrix(as.formula(paste0("~ ", model$call$model[3])),
                     data = newdata)
  vars <- mm %*% vcov(model) %*% t(mm)
  sds <- sqrt(diag(vars))
  newdata <- newdata %>% mutate(lowCI = Predict - 1.96 * sds,
                                HiCI = Predict + 1.96 * sds)
}


printCoefficients <- function(model){
  as_tibble(trimws(format(round(summary(model)$tTable, 3), nsmall=3))) %>%
    mutate(Coefficient = rownames(summary(model)$tTable)) %>% 
    select(Coefficient, Value, Std.Error, 'p-value') %>% 
    print()
}

# all.UK.rates - Import conception rates for GBR countries -------------------------------------------

all.UK.rates <-
  read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 18")


# Create datasets --------------------------------------------------------------------------------------------


#** EngData - England only -------------------------------------------------------------------------------------

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

#** EngScotContro data setup - Eng v Scot -----------------------------------------------

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
  mutate_at(., colnames(.)[5:9], list(Eng = ~ .*England))  # Potentially not necessary

#** EngWalContro data setup - Eng vs Wales as control ----------------------------------------------------------

EngWalContro <-
  all.UK.rates %>% filter(Country == "Wales" |
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
  mutate_at(., colnames(.)[5:9], list(Eng = ~ .*England))  # Potentially not necessary

# Visualise all UK rates -------------------------------------------------------------------------------------


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


EngData %>% ggplot(aes(x = Year, y = Value, group = Cat1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

EngMod <- lm(Value ~ Time + Cat1 + Trend1, data = EngData)
summary(EngMod)

# Test for autocorrelation

testAutocorr(EngMod, max.lag = 15)


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

testAutocorr(EngMod2, max.lag = 15)  # Potential MA3?

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


# EngModPI99_07 - phase-in with two interventions ------------------------------------------------------------

lm(
  Value ~ Time +
    Cat1 +
    Trend1 +
    Cat2 +
    Trend2,
  data = {EngData %>% 
      filter(Year < 1999 | Year > 2000) %>% 
      mutate(Trend1=ifelse(Cat1==0,0,Trend1-2))}
) %>%
  assign("EngModPI_99_07", ., envir = .GlobalEnv) %T>%
  {print(ggplot(data=eval(.$call$data), aes(
    Year,
    Value,
    group = interaction(Cat1, Cat2)
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
  } %>%
  summary()


## modScot99 - Comparing for pre-post 1999 --------------------------------------------

lm(Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
     data = EngScotContro) %>%
  assign("modScot99", ., envir = .GlobalEnv) %T>%
  summary() %>% 
{  print(ggplot(eval(.$call$data), aes(
    Year, Value, group = interaction(Country, Cat1), col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
}

testAutocorr(modScot99)  # Decay in ACF, sig at lag=3 in PACF. Indicates AR3

modScot99_p3 <-
  gls(
    Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
    data = EngScotContro,
    correlation = corARMA(p = 3, form =  ~ Time | England),
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

# modScot99_p3_cfac <- tibble(
#   Time = c(8:25),
#   Trend1 = c(1:18),
#   Value = (
#     modScot99_p3$coefficients[1] +
#       modScot99_p3$coefficients[2] * Time +
#       modScot99_p3$coefficients[3] +
#       modScot99_p3$coefficients[4] * Time +
#       modScot99_p3$coefficients[5] +
#       modScot99_p3$coefficients[6] * Trend1
#   )
# )

modScot99_null <- 
  gls(
    Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
    data = EngScotContro,
    correlation = NULL,
    method = "ML"
  )
summary(modScot99_null)
confint(modScot99_null)

modScot99_null_cfac <- tibble(
  Time=c(8:25),
  Cat1=1,
  Trend1=c(1:18),
  England=1,
  Time_Eng=c(8:25),
  Cat1_Eng=0,
  Trend1_Eng=0
)

modScot99_null_cfac <- left_join(modScot99_null_cfac, constructCIRibbon(modScot99_null_cfac, modScot99_null))

EngScotContro %>%
  mutate(predict = predict(modScot99_null)) %>% 
  ggplot(aes(Time, Value, col = Country, group = interaction(Country, Cat1))) +
  geom_point() +
  geom_smooth(
    data = modScot99_null_cfac,
    aes(x = Time,
        y = Value,
    ymax = HiCI,
    ymin=lowCI),
    stat="identity",
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
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0))



## modScot99_07 - Comparing for three stages, split at 1999 and 2007 --------------------

#** Initial model -----

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
  data = EngScotContro
) %>%
  assign("modScot99_07", ., envir = .GlobalEnv) %T>%
  {print(ggplot(data=eval(.$call$data), aes(
      Year,
      Value,
      group = interaction(Country, Cat1, Cat2),
      col = Country
    )) +
      geom_point() + geom_smooth(method = "lm", se = FALSE))
  } %>%
  summary()

## test autocorrelation
testAutocorr(modScot99_07)  # Assume NULL for now

#** New model -----

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

str(summary(modScot99_07_null))

confint(modScot99_07_null)

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

#** Graphing final model -----

EngScotContro %>%
  left_join(constructCIRibbon((filter(., England==1, Year>1998)), modScot99_07_null)) %>%  # England CI ribbon
  mutate(Predict = predict(modScot99_07_null)) %>%  # Add Predicts for non-England
  ggplot(aes(
    Time,
    Value,
    col = Country,
    fill = Country,
    group = interaction(Country, Cat1, Cat2)
  )) +
  # Show all data points
  geom_point(data=EngScotContro, show.legend = FALSE) +
  # Counterfactual trend lines
  geom_line(
    data = modScot99_07_null_cfac,
    aes(
      x = Time,
      y = Predict,
      group = Cat2,
      col = "Control",
      fill = NULL
    ),
    linetype = "longdash",
    size = 1,
    inherit.aes = FALSE
  ) +
  # Counterfactual confidence intervals (not shown in legend)
  geom_ribbon(
    data=modScot99_07_null_cfac,
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      group = Cat2,
      col=NULL,
      fill="Control"
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE,
    inherit.aes = FALSE) +
  # Model trend lines
  geom_line(aes(y=Predict), size = 1) +
  # Confidence intervals (not shown in legend)
  geom_ribbon(
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      col=NULL,
      fill=Country
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE) +
  # Intervention time points
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  geom_vline(xintercept = 16.5,
             linetype = "dotted",
             col = "#000000CC") +
  # Display parameters
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  theme(panel.background = element_blank(),
        legend.key  = element_blank(),
        panel.grid = element_blank()) +
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(
    breaks = c("England", "Wales", "Scotland", "Control"),
    values = c("Wales" = "#00AB39",
               "Scotland" = "#0072C6",
               "England" = "#CF142B",
               "Control" = "#F7D917"),
    aesthetics = c("colour", "fill"))

ggsave("graphs/Scot99_07.png")

# modScotPI - Comparing pre-1999 and post-2007, proposing phase-in -------------------------------------------

#** Initial model -----

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

#** New model -----

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

#** Graphing final model -----

EngScotContro %>%
  filter(Year < 1999 | Year > 2007) %>%
  left_join(constructCIRibbon((filter(., England==1, Year>2007)), modScotPI_null)) %>%  # England CI ribbon
  mutate(Predict = predict(modScotPI_null)) %>%
  ggplot(aes(Time, Value, col = Country, group = interaction(Country, Cat2))) +
  # Show all data points
  geom_point(data=EngScotContro, show.legend = FALSE) +
  # Counterfactual trend lines
  geom_line(
    data = modScotPI_null_cfac,
    aes(
      x = Time,
      y = Predict,
      col = "Control",
      fill = NULL
    ),
    linetype = "longdash",
    size = 1,
    inherit.aes = FALSE
  ) +
  # Counterfactual confidence intervals (not shown in legend)
  geom_ribbon(
    data=modScotPI_null_cfac,
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      col=NULL,
      fill="Control"
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE,
    inherit.aes = FALSE) +
  # Model trend lines
  geom_line(aes(y=Predict), size = 1) +
  # Confidence intervals (not shown in legend)
  geom_ribbon(
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      col=NULL,
      fill=Country
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE) +
  # Intervention time points
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  # Phase-in period greyed out
  geom_rect(
    xmin = 7.5,
    xmax = 16.5,
    ymin = 0,
    ymax = 60,
    fill = "grey",
    alpha = 0.01,
    inherit.aes = FALSE
  ) +
  # Display parameters
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  theme(panel.background = element_blank(),
        legend.key  = element_blank()) +
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(
    breaks = c("England", "Wales", "Scotland", "Control"),
    values = c("Wales" = "#00AB39",
               "Scotland" = "#0072C6",
               "England" = "#CF142B",
               "Control" = "#F7D917"),
    aesthetics = c("colour", "fill"))

ggsave("graphs/Scot_99_07.png")

# modScotPI_99_07 - comparing Eng vs Scot with phase-in and two interventions --------------------------------

#** Initial model -----

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
  data = {EngScotContro %>% 
      filter(Year < 1999 | Year > 2000) %>% 
      mutate(Trend1=ifelse(Cat1==0,0,Trend1-2),
             Trend1_Eng=Trend1*England)}
) %>%
  assign("modScotPI_99_07", ., envir = .GlobalEnv) %T>%
  {print(ggplot(data=eval(.$call$data), aes(
    Year,
    Value,
    group = interaction(Country, Cat1, Cat2),
    col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
  } %>%
  summary()

## test autocorrelation
testAutocorr(modScotPI_99_07, time.points = 23)  # Assume NULL for now

#** New model -----
modScotPI_99_07_null <- gls(
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
  data = {EngScotContro %>% 
      filter(Year < 1999 | Year > 2000) %>% 
      mutate(Trend1=ifelse(Cat1==0,0,Trend1-2),
             Trend1_Eng=Trend1*England)},
  correlation = NULL,
  method = "ML"
)

summary(modScotPI_99_07_null)

confint(modScotPI_99_07_null)


modScotPI_99_07_null_cfac <- EngScotContro[10:25, ] %>%
  select(Time, England, Cat1, Cat2, Trend2, Time_Eng) %>%
  mutate(
    Trend1 = c(1:16),
    Cat1_Eng = c(rep(0, 7), rep(1, 9)),
    Trend1_Eng = c(rep(0, 7), 8:16),
    Cat2_Eng = 0,
    Trend2_Eng = 0
  )

modScotPI_99_07_null_cfac <-
  modScotPI_99_07_null_cfac %>%
  left_join(constructCIRibbon(modScotPI_99_07_null_cfac, modScotPI_99_07_null))

#** Graphing final model -----

EngScotContro %>%
  filter(Year < 1999 | Year > 2000) %>%
  mutate(Trend1=ifelse(Cat1==0,0,Trend1-2),
         Trend1_Eng=Trend1*England) %>% 
  left_join(constructCIRibbon((filter(., England==1, Year>2000)), modScotPI_99_07_null)) %>%  # England CI ribbon
  mutate(Predict = predict(modScotPI_99_07_null)) %>%  # Add Predicts for non-England
  ggplot(aes(
    Time,
    Value,
    col = Country,
    fill=Country,
    group = interaction(Country, Cat1, Cat2)
  )) +
  # Show all data points
  geom_point(data=EngScotContro, show.legend = FALSE) +
  # Counterfactual trend lines
  geom_line(
    data = modScotPI_99_07_null_cfac,
    aes(
      x = Time,
      y = Predict,
      group = Cat2,
      col = "Control",
      fill = NULL
    ),
    linetype = "longdash",
    size = 1,
    inherit.aes = FALSE
  ) +
  # Counterfactual confidence intervals (not shown in legend)
  geom_ribbon(
    data=modScotPI_99_07_null_cfac,
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      group = Cat2,
      col=NULL,
      fill="Control"
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE,
    inherit.aes = FALSE) +
  # Model trend lines
  geom_line(aes(y=Predict), size = 1) +
  # Confidence intervals (not shown in legend)
  geom_ribbon(
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      col=NULL,
      fill=Country
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE) +
  # Intervention time points
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  geom_vline(xintercept = 16.5,
             linetype = "dotted",
             col = "#000000CC") +
  # Phase-in period greyed out
  geom_rect(
    xmin = 7.5,
    xmax = 9.5,
    ymin = 0,
    ymax = 60,
    fill = "grey",
    alpha = 0.01,
    inherit.aes = FALSE
  ) +
  # Display parameters
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  theme(panel.background = element_blank(),
        legend.key  = element_blank()) +
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(
    breaks = c("England", "Wales", "Scotland", "Control"),
    values = c("Wales" = "#00AB39",
               "Scotland" = "#0072C6",
               "England" = "#CF142B",
               "Control" = "#F7D917"),
    aesthetics = c("colour", "fill"))

ggsave("graphs/ScotPI_99_07.png")

# modWal99 - Comparing for changes pre-post 1999 -------------------------------------------------------------

lm(Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
   data = EngWalContro) %>%
  assign("modWal99", ., envir = .GlobalEnv) %T>%
  summary() %>% 
  {  print(ggplot(eval(.$call$data), aes(
    Year, Value, group = interaction(Country, Cat1), col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
  }

testAutocorr(modWal99)

modWal99_null <- 
  gls(
    Value ~ Time + England + Time_Eng + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng,
    data = EngWalContro,
    correlation = NULL,
    method = "ML"
  )
summary(modWal99_null)
confint(modWal99_null)

modWal99_null_cfac <- tibble(
  Time=c(8:25),
  Cat1=1,
  Trend1=c(1:18),
  England=1,
  Time_Eng=c(8:25),
  Cat1_Eng=0,
  Trend1_Eng=0
)

modWal99_null_cfac <- left_join(modWal99_null_cfac, constructCIRibbon(modWal99_null_cfac, modWal99_null))

EngWalContro %>%
  mutate(predict = predict(modWal99_null)) %>% 
  ggplot(aes(Time, Value, col = Country, group = interaction(Country, Cat1))) +
  geom_point() +
  geom_smooth(
    data = modWal99_null_cfac,
    aes(x = Time,
        y = Value,
        ymax = HiCI,
        ymin=lowCI),
    stat="identity",
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
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0))

# modWal99_07 - Comparing for changes at 1999 and 2007 -------------------------------------------------------

#** Initial model -----

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
  data = EngWalContro
) %>%
  assign("modWal99_07", ., envir = .GlobalEnv) %T>%
  {print(ggplot(data=eval(.$call$data), aes(
    Year,
    Value,
    group = interaction(Country, Cat1, Cat2),
    col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
  } %>%
  summary()

## test autocorrelation
testAutocorr(modWal99_07)  # Assume NULL for now

#** New model -----

modWal99_07_null <- gls(
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
  data = EngWalContro,
  correlation = NULL,
  method = "ML"
)

summary(modWal99_07_null)

confint(modWal99_07_null)

modWal99_07_null_cfac <- tibble(
  Time = c(8:25),
  England = 1,
  Time_Eng = c(8:25),
  Cat1 = 1,
  Trend1 = c(1:18),
  Cat2 = c(rep(0,9), rep(1,9)),
  Trend2 = c(rep(0,9), 1:9),
  Cat1_Eng = c(rep(0,9), rep(1,9)),
  Trend1_Eng = c(rep(0,9), 10:18),
  Cat2_Eng = 0,
  Trend2_Eng = 0
  )  # Remove _Eng interactions (retaining 1st intervention interactions for 2nd int)

modWal99_07_null_cfac <-
  modWal99_07_null_cfac %>%
  left_join(constructCIRibbon(modWal99_07_null_cfac, modWal99_07_null))

#** Graphing final model -----

EngWalContro %>%
  left_join(constructCIRibbon((filter(., England==1, Year>1998)), modWal99_07_null)) %>%  # England CI ribbon
  mutate(Predict = predict(modWal99_07_null)) %>%  # Add Predicts for non-England
  ggplot(aes(
    Time,
    Value,
    col = Country,
    fill = Country,
    group = interaction(Country, Cat1, Cat2)
  )) +
  # Show all data points
  geom_point(data=EngWalContro, show.legend = FALSE) +
  # Counterfactual trend lines
  geom_line(
    data = modWal99_07_null_cfac,
    aes(
      x = Time,
      y = Predict,
      group = Cat2,
      col = "Control",
      fill = NULL
    ),
    linetype = "longdash",
    size = 1,
    inherit.aes = FALSE
  ) +
  # Counterfactual confidence intervals (not shown in legend)
  geom_ribbon(
    data=modWal99_07_null_cfac,
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      group = Cat2,
      col=NULL,
      fill="Control"
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE,
    inherit.aes = FALSE) +
  # Model trend lines
  geom_line(aes(y=Predict), size = 1) +
  # Confidence intervals (not shown in legend)
  geom_ribbon(
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      col=NULL,
      fill=Country
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE) +
  # Intervention time points
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  geom_vline(xintercept = 16.5,
             linetype = "dotted",
             col = "#000000CC") +
  # Display parameters
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  theme(panel.background = element_blank(),
        legend.key  = element_blank()) +
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(
    breaks = c("England", "Wales", "Scotland", "Control"),
    values = c("Wales" = "#00AB39",
               "Scotland" = "#0072C6",
               "England" = "#CF142B",
               "Control" = "#F7D917"),
    aesthetics = c("colour", "fill"))

ggsave("graphs/Wal99_07.png")

# modWalPI - Comparing pre-1999 and post 2007 (assuming phase-in) --------------------------------------------

#** Initial model -----

lm(Value ~ Time + England + Time_Eng + Cat2 + Trend2 + Cat2_Eng + Trend2_Eng,
   data = {EngWalContro %>% filter(Year < 1999 | Year > 2007)}) %>%
  assign("modWalPI", ., envir = .GlobalEnv) %T>%
  {print(ggplot(data = eval(.$call$data), aes(
    Year,
    Value,
    group = interaction(Country, Cat1, Cat2),
    col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))} %>% 
  summary()

testAutocorr(modWalPI, max.lag=14, time.points = 16)  # Possibly no autocorrelation? Sig at PACF 12 and 13

#** New model -----

modWalPI_null <-
  gls(
    Value ~ Time + England + Time_Eng + Cat2 + Trend2 + Cat2_Eng + Trend2_Eng,
    data = EngWalContro %>% filter(Year < 1999 | Year > 2007),
    correlation = NULL,
    method = "ML"
  )

summary(modWalPI_null)
confint(modWalPI_null)

modWalPI_null_cfac <- tibble(
  Time = c(17:25),
  England = 1,
  Cat2 = 1,
  Trend2 = c(1:9),
  Time_Eng = c(17:25),
  Cat2_Eng = 0,
  Trend2_Eng = 0
)  # Emlulating England with no added effect

modWalPI_null_cfac <-
  modWalPI_null_cfac %>%
  left_join(constructCIRibbon(modWalPI_null_cfac, modWalPI_null))

#** Graphing final model -----

EngWalContro %>%
  filter(Year < 1999 | Year > 2007) %>%
  left_join(constructCIRibbon((filter(., England==1, Year>2007)), modWalPI_null)) %>%  # England CI ribbon
  mutate(Predict = predict(modWalPI_null)) %>%
  ggplot(aes(Time, Value, col = Country, group = interaction(Country, Cat2))) +
  # Show all data points
  geom_point(data=EngWalContro, show.legend = FALSE) +
  # Counterfactual trend lines
  geom_line(
    data = modWalPI_null_cfac,
    aes(
      x = Time,
      y = Predict,
      col = "Control",
      fill = NULL
    ),
    linetype = "longdash",
    size = 1,
    inherit.aes = FALSE
  ) +
  # Counterfactual confidence intervals (not shown in legend)
  geom_ribbon(
    data=modWalPI_null_cfac,
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      col=NULL,
      fill="Control"
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE,
    inherit.aes = FALSE) +
  # Model trend lines
  geom_line(aes(y=Predict), size = 1) +
  # Confidence intervals (not shown in legend)
  geom_ribbon(
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      col=NULL,
      fill=Country
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE) +
  # Intervention time points
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  # Phase-in period greyed out
  geom_rect(
    xmin = 7.5,
    xmax = 16.5,
    ymin = 0,
    ymax = 60,
    fill = "grey",
    alpha = 0.01,
    inherit.aes = FALSE
  ) +
  # Display parameters
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  theme(panel.background = element_blank(),
        legend.key  = element_blank()) +
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(
    breaks = c("England", "Wales", "Scotland", "Control"),
    values = c("Wales" = "#00AB39",
               "Scotland" = "#0072C6",
               "England" = "#CF142B",
               "Control" = "#F7D917"),
    aesthetics = c("colour", "fill"))

ggsave("graphs/WalPI.png")

# modWalPI_99_07 - comparing Eng vs Wal with phase-in period and two interventions ---------------------------

#** Initial model -----

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
  data = {EngWalContro %>% 
      filter(Year < 1999 | Year > 2000) %>% 
      mutate(Trend1=ifelse(Cat1==0,0,Trend1-2),
             Trend1_Eng=Trend1*England)}
) %>%
  assign("modWalPI_99_07", ., envir = .GlobalEnv) %T>%
  {print(ggplot(data=eval(.$call$data), aes(
    Year,
    Value,
    group = interaction(Country, Cat1, Cat2),
    col = Country
  )) +
    geom_point() + geom_smooth(method = "lm", se = FALSE))
  } %>%
  summary()

## test autocorrelation
testAutocorr(modWalPI_99_07, time.points = 23)  # Assume NULL for now

#** New model -----

modWalPI_99_07_null <- gls(
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
  data = {EngWalContro %>% 
      filter(Year < 1999 | Year > 2000) %>% 
      mutate(Trend1=ifelse(Cat1==0,0,Trend1-2),
             Trend1_Eng=Trend1*England)},
  correlation = NULL,
  method = "ML"
)

summary(modWalPI_99_07_null)

confint(modWalPI_99_07_null)


modWalPI_99_07_null_cfac <- EngWalContro[10:25, ] %>%
  select(Time, England, Cat1, Cat2, Trend2, Time_Eng) %>%
  mutate(
    Trend1 = c(1:16),
    Cat1_Eng = c(rep(0, 7), rep(1, 9)),
    Trend1_Eng = c(rep(0, 7), 8:16),
    Cat2_Eng = 0,
    Trend2_Eng = 0
  )

modWalPI_99_07_null_cfac <-
  modWalPI_99_07_null_cfac %>%
  left_join(constructCIRibbon(modWalPI_99_07_null_cfac, modWalPI_99_07_null))

#** Graphing final model -----

EngWalContro %>%
  filter(Year < 1999 | Year > 2000) %>%
  mutate(Trend1=ifelse(Cat1==0,0,Trend1-2),
         Trend1_Eng=Trend1*England) %>% 
  left_join(constructCIRibbon((filter(., England==1, Year>2000)), modWalPI_99_07_null)) %>%  # England CI ribbon
  mutate(Predict = predict(modWalPI_99_07_null)) %>%  # Add Predicts for non-England
  ggplot(aes(
    Time,
    Value,
    col = Country,
    fill=Country,
    group = interaction(Country, Cat1, Cat2)
  )) +
  # Show all data points
  geom_point(data=EngWalContro, show.legend = FALSE) +
  # Counterfactual trend lines
  geom_line(
    data = modWalPI_99_07_null_cfac,
    aes(
      x = Time,
      y = Predict,
      group = Cat2,
      col = "Control",
      fill = NULL
    ),
    linetype = "longdash",
    size = 1,
    inherit.aes = FALSE
  ) +
  # Counterfactual confidence intervals (not shown in legend)
  geom_ribbon(
    data=modWalPI_99_07_null_cfac,
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      group = Cat2,
      col=NULL,
      fill="Control"
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE,
    inherit.aes = FALSE) +
  # Model trend lines
  geom_line(aes(y=Predict), size = 1) +
  # Confidence intervals (not shown in legend)
  geom_ribbon(
    aes(
      x=Time,
      ymin = lowCI,
      ymax=HiCI,
      col=NULL,
      fill=Country
    ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE) +
  # Intervention time points
  geom_vline(xintercept = 7.5,
             linetype = "dotted",
             col = "#000000CC") +
  geom_vline(xintercept = 16.5,
             linetype = "dotted",
             col = "#000000CC") +
  # Phase-in period greyed out
  geom_rect(
    xmin = 7.5,
    xmax = 9.5,
    ymin = 0,
    ymax = 60,
    fill = "grey",
    alpha = 0.01,
    inherit.aes = FALSE
  ) +
  # Display parameters
  scale_x_continuous(breaks = c(4, 9, 14, 19, 24),
                     labels = seq(1995, 2015, by = 5)) +
  theme(panel.background = element_blank(),
        legend.key  = element_blank()) +
  ylab("Rate of pregnancies to under-18s, per 1,000") +
  xlab("Year") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(
    breaks = c("England", "Wales", "Scotland", "Control"),
    values = c("Wales" = "#00AB39",
               "Scotland" = "#0072C6",
               "England" = "#CF142B",
               "Control" = "#F7D917"),
    aesthetics = c("colour", "fill"))

ggsave("graphs/WalPI_99_07.png")
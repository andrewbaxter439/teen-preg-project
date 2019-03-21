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

## new model
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
    Country = "Control",
    Trend1 = c(1:16),
    Cat1_Eng = c(rep(0, 7), rep(1, 9)),
    Trend1_Eng = c(rep(0, 7), 8:16),
    Cat2_Eng = 0,
    Trend2_Eng = 0
  )

modWalPI_99_07_null_cfac <-
  modWalPI_99_07_null_cfac %>%
  left_join(constructCIRibbon(modWalPI_99_07_null_cfac, modWalPI_99_07_null))

## Graphing final model

EngWalContro %>%
  filter(Year < 1999 | Year > 2000) %>%
  mutate(Trend1=ifelse(Cat1==0,0,Trend1-2),
         Trend1_Eng=Trend1*England) %>% 
  left_join(constructCIRibbon((filter(., England==1, Year>2000)), modWalPI_99_07_null)) %>% 
  mutate(Predict = predict(modWalPI_99_07_null)) %>%
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
      fill=Country
      ),
    alpha=0.5,
    size = 1,
    show.legend = FALSE,
    inherit.aes = FALSE) +
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
  # Model trend lines
  geom_line(aes(y=Predict), size = 1) +
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
    values = c("Wales" = "#00AB39",
               "Scotland" = "F7D917",
               "England" = "#CF142B",
               "Control" = "#F7D917"),
    aesthetics = c("colour", "fill"))

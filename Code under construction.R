library(tidyverse)
library(readxl)

Scot.data <- read_xlsx("Downloaded data files/AllUK1994-2016.xlsx", sheet = "scot_agegrp")

ab20.correction <- Scot.abort %>% 
  left_join(Scot.data, by=c("Year", "agegrp")) %>% 
  select(Year, Abortions.x, Abortions.y) %>% 
  filter(!is.na(Abortions.y)) %>% 
  summarise(factor = summary(lm(Abortions.x ~ 0 + Abortions.y, data=.))[["coefficients"]][1])



# Visualise changing share in abortion across time -----------------------------------------------------------

Scot.data %>% 
  ggplot(aes(Year, Abortions, fill = agegrp)) +
  geom_bar(position = "fill", stat = "identity")

Scot.data %>% 
  ggplot(aes(Year, Percent_abortion, col = agegrp)) +
  geom_line()

Scot.data %>% 
  select(Year, agegrp, Abortions, Deliveries) %>% 
  filter(agegrp != 'Under 20') %>% 
  left_join(Scot.data %>% filter(agegrp == 'Under 20') %>% select(Year, "U20ab" = Abortions), by = "Year") %>% 
  group_by(agegrp) %>% 
  group_map(., ~ tibble(
    Beta1 = summary(lm(Abortions ~ U20ab + Year, data = .))[["coefficients"]][[2]],
    Beta2 = summary(lm(Abortions ~ U20ab + Year, data = .))[["coefficients"]][[3]],
    # Beta3 = summary(lm(Abortions ~ U20ab + Year, data = .))[["coefficients"]][[4]],
                        Rsqr = summary(lm(Abortions ~ U20ab + Year, data = .))[["r.squared"]]))

# Under 18 model has high R^2 (0.991), under 16 model low (0.877). Adding number of deliveries does not increase
# R^2, so left out of model

ScotU18.data <- Scot.data %>% 
  select(Year, agegrp, Abortions, Deliveries) %>% 
  filter(agegrp == 'Under 18') %>% 
  left_join(Scot.data %>% filter(agegrp == 'Under 20') %>% select(Year, U20ab = Abortions), by = "Year") 

ScotU18abort.model <- lm(Abortions ~ U20ab + Year, data = ScotU18.data)

# Test addition of deliveries (No significant difference)
update(ScotU18abort.model, .~. + Deliveries) %>% anova(ScotU18abort.model)

# Test abortion estimation for Under 18s ---------------------------------------------------------------------

ScotU18.data %>% 
  mutate(Abortions.corr = predict(ScotU18abort.model,.),
         Rsqr = ifelse(Year == 2010, 
                       paste0("R^2 = ", round(cor(Abortions, Abortions.corr)^2, 3)),
                       NA)) %>%
  ggplot(aes(Year, Abortions)) +
  geom_line(aes(col = "Abortions recorded")) +
  geom_line(aes(y = Abortions.corr, col = "Abortions predicted")) +
  geom_text(aes(label = Rsqr), hjust = -0.5)


# Predict Under18 abortions for 1985 to 1993 -----------------------------------------------------------------

 scotConceptionsUnde18 <-
  sumScotBirths %>% 
  filter(agegrp == "Under 18") %>% 
  select(Year, agegrp, Deliveries.corr) %>% 
  left_join(Scot.abort %>% select(Year, U20ab = Abortions.corr), by = "Year") %>% 
  mutate(Abortions.corr = predict(ScotU18abort.model,.),
         Total = Deliveries.corr + Abortions.corr)%>% 
  left_join(sumScot.pop, by = c("Year", "agegrp")) %>% 
  mutate(Value = 1000*Total/Population)

write_csv(scotConceptionsUnde18, "EstScotU18_1985_2015.csv")

ggplot(scotConceptionsUnde18, aes(Year, Value)) +
         geom_line(aes(col = "Predicted")) +
         geom_line(data = Scot.data %>% filter(agegrp == "Under 18"), aes(y = Con_rate, col = "Recorded"))

#*** Temp - check out lms of predicting pre-1994 abortions -----------------------------

Scot.data %>% 
  select(Year, agegrp, Abortions, Deliveries) %>% 
  filter(agegrp == 'Under 18') %>% 
  left_join(Scot.data %>% 
              filter(agegrp == 'Under 20') %>%
              select(Year, "U20ab" = Abortions, "U20del" = Deliveries), by = "Year") %>% 
  ggplot(aes(Year, Deliveries, group = agegrp)) +
  geom_line(aes(col="Deliveries recorded"), size = 1) +
  geom_line(aes(y = Abortions, col="Abortions to under 18s"), size = 1) +
  geom_line(aes(y = U20ab, col = "Abortions to under 20s"), linetype = "dashed", size = 1) +
  geom_line(aes(y = U20del, col = "Deliveries to under 20s"), linetype = "dashed", size = 1)
       

Scot.data %>% 
  select(Year, agegrp, Abortions, Deliveries) %>% 
  filter(agegrp == 'Under 18') %>% 
  left_join(Scot.data %>% 
              filter(agegrp == 'Under 20') %>% 
              select(Year, "U20ab" = Abortions, "U20del" = Deliveries), by = "Year") %>% 
  mutate(U20ratio = U20ab/U20del, U20prop = U20ab/(U20ab+U20del)) %T>% 
  {lm(Abortions ~ Year  + Deliveries + U20ab + U20del + U20prop, .) %>% summary() %>% print()} %>% 
  {lm(Abortions ~ U20ab + Year, .) %>% summary()}
  
Scot.data %>% 
  select(Year, agegrp, Abortions, Deliveries) %>% 
  filter(agegrp == 'Under 18') %>% 
  left_join(Scot.data %>% 
              filter(agegrp == 'Under 20') %>%
              select(Year, "U20ab" = Abortions, "U20del" = Deliveries), by = "Year") %>% 
  mutate(U20ratio = U20ab/U20del, U20prop = U20ab/(U20ab+U20del)) %>% 
  ggplot(aes(Abortions, y = U20ab/Year)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) + 
  geom_text(aes(label = Year))

Scot.data %>% 
  select(Year, agegrp, Abortions, Deliveries) %>% 
  filter(agegrp == 'Under 18') %>% 
  left_join(Scot.data %>% 
              filter(agegrp == 'Under 20') %>%
              select(Year, "U20ab" = Abortions, "U20del" = Deliveries), by = "Year") %>% 
  mutate(U20ratio = U20ab/U20del, U20prop = U20ab/(U20ab+U20del)) %>% 
  plot_ly(x = ~ Abortions,
          y = ~ U20ab,
          z = ~ U20ab*U20prop) %>% 
  add_markers()

### Equation calculation from Under20 abortion and Year seems to be best fit for fewest predictors ###


# England predict pre-1992 -----------------------------------------------------------------------------------

Eng.pop <- read_fwf("Downloaded data files/Population/GBRTENW.Population.txt",
                    skip = 3,
                    fwf_cols(Year = c(3, 6), Age = c(14, 20), Total = c(31, 40)))

Eng.pregs <- read_xlsx("Downloaded data files/AllUK1994-2016.xlsx", sheet = "by_agegrp") %>% 
  filter(Country == "England")



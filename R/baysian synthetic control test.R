library(CausalImpact)
library(tidyverse)

zoo_u18_sp <- md_u18_sp %>% 
  pivot_wider(Year, names_from = "Group", values_from = "Rate") %>% 
  read.zoo()

impact <- CausalImpact(zoo_u18_sp, pre.period = c(1990, 1998), post.period = c(1999, 2013))

plot(impact)

summary(impact)

dp_u18_all
so_u18_all

zoo_u18_sp2 <- synthData_u18_filt %>% 
  mutate(Country = str_replace_all(Country, " ", "_")) %>% 
  dplyr::select(Year, Country, rate) %>% 
  pivot_wider(Year, names_from = "Country", values_from = "rate") %>% 
  dplyr::select(Year, England_and_Wales, 2, 4:17) %>% 
  read.zoo()


impact <- CausalImpact(zoo_u18_sp2, pre.period = c(1990, 1998), post.period = c(1999, 2013))

plot <- plot(impact)

summary(impact)

zoo_u20_sp2 <- synthData_u20_filt %>% 
  mutate(Country = str_replace_all(Country, " ", "_")) %>% 
  dplyr::select(Year, Country, pRate) %>% 
  pivot_wider(Year, names_from = "Country", values_from = "pRate") %>% 
  dplyr::select(Year, England_and_Wales, 2, 4:17) %>% 
  read.zoo()


impact2 <- CausalImpact(zoo_u20_sp2, pre.period = c(1990, 1998), post.period = c(1999, 2013))

plot(impact2)

summary(impact2)

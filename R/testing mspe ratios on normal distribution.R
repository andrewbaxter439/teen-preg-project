library(tidyverse)

log_rank <- rank_u18 %>% 
  mutate(log_ratio = log(`Post/pre MSPE ratio`)) %>% 
  select(Country, log_ratio)


dist_params <- log_rank %>%
  filter(Country != "England and Wales") %>% 
  summarise(mean = mean(log_ratio),
            sd = sd(log_ratio))

Eng_ratio <- log_rank %>% 
  filter(Country == "England and Wales") %>% 
  pull(log_ratio)

ggplot(tibble(x = -3:7), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(dist_params$mean, dist_params$sd)) +
  geom_density(aes(log_ratio), log_rank, colour = "red") +
  geom_vline(xintercept = Eng_ratio, colour = "blue")


pnorm(Eng_ratio, dist_params$mean, dist_params$sd, lower.tail = FALSE)

qnorm(0.05, dist_params$mean, dist_params$sd, lower.tail = FALSE)


## Under20 ---------------------------------------------------------------


log_rank <- rank_u20 %>% 
  mutate(log_ratio = log(`Post/pre MSPE ratio`)) %>% 
  select(Country, log_ratio)


dist_params <- log_rank %>%
  filter(Country != "England and Wales") %>% 
  summarise(mean = mean(log_ratio),
            sd = sd(log_ratio))

Eng_ratio <- log_rank %>% 
  filter(Country == "England and Wales") %>% 
  pull(log_ratio)

ggplot(tibble(x = -3:7), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(dist_params$mean, dist_params$sd)) +
  geom_density(aes(log_ratio), log_rank, colour = "red") +
  geom_vline(xintercept = Eng_ratio, colour = "blue")


pnorm(Eng_ratio, dist_params$mean, dist_params$sd, lower.tail = FALSE)

qnorm(0.05, dist_params$mean, dist_params$sd, lower.tail = FALSE)

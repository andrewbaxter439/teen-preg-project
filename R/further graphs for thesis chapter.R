# setup (more than needed) ------------------------------------------------
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
library(gganimate)
library(ggrepel)
library(patchwork)

load('Data/synth_data_c.rdata') # outputted from 'Synth_data.R'
# load('Data/synth_data_b.rdata') # outputted from 'Synth_data.R'
load("Data/filtered_itsp.rdata")  # outputted from 'Synth_create_sps.R'
load("Data/placebo_country_b.rdata")  # outputted from 'Synth_create_sps.R'
load("Data/time_placebos_b.rdata")  # outputted from 'Synth_time_pbs.R'
# load('Data/noScot.rdata')
load("Data/other_predictors.rdata")
source('R/Synth_functions.R')



# saving faceted placebo times --------------------------------------------

super_md_u18_filt %>% 
  filter(IntYr < 1999) %>% 
  ggplot(aes(Year, Rate, col = Group, linetype = Group, frame = IntYr)) +
  geom_segment(aes(x = IntYr-0.5, xend = IntYr-0.5, y = 0, yend = 20), linetype = "dotted", size = 1, col = "grey", alpha = 0.7) +
  geom_line(size = 1.5) +
  theme_sphsu_light() +
  ylab(paste0("Under-18 birth rate (per 1,000 women)")) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_line(colour = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        title = element_text(size = 12)
        ) +
  scale_linetype_manual(name = "Data", values = c("Synthetic" = "solid", "Treated" = "solid")) +
  scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
  labs(title = "England vs Synthetic Control with placebo intervention years") +
  scale_y_continuous(limits = c(0, 20)) +
  facet_wrap(~ IntYr)

ggsave("graphs/u18_pbtime_sp.png", dpi = 400, width = 155, height = 100, units = "mm")



## u20 --------------------------------------------------------------------

super_md_u20_filt %>% 
  filter(IntYr < 1999) %>% 
  ggplot(aes(Year, Rate, col = Group, linetype = Group, frame = IntYr)) +
  geom_segment(aes(x = IntYr-0.5, xend = IntYr-0.5, y = 0, yend = 80), linetype = "dotted", size = 1, col = "grey", alpha = 0.7) +
  geom_line(size = 1.5) +
  theme_sphsu_light() +
  ylab(paste0("Under-20 pregnancy rate (per 1,000 women)")) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_line(colour = "#e0e0e0"),
        panel.grid.minor = element_blank()
        ) +
  scale_linetype_manual(name = "Data", values = c("Synthetic" = "solid", "Treated" = "solid")) +
  scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
  labs(title = "England vs Synthetic Control with placebo intervention years") +
  scale_y_continuous(limits = c(0, 80)) +
  facet_wrap(~ IntYr)

ggsave("graphs/u20_pbtime_sp.png", dpi = 400, width = 155, height = 100, units = "mm")



# joint graphs of all predictors ------------------------------------------

library(patchwork)

u18_left <- gr_u18_all
u18_right <- pb_plot_u18_all

u18_left[["layers"]][[1]][["aes_params"]]$size <- 1
u18_right[["layers"]][[3]][["aes_params"]]$size <- 1


((u18_left + u18_right) & 
    theme(legend.position = "bottom")) / 
  guide_area() + 
  plot_layout(guides = 'collect',
              width = 155,
              heights = c(80, 5),
              )

ggsave("graphs/u18_all.png", dpi = 400, width = 155, height = 85, units = "mm")


u20_left <- gr_u20_all
u20_right <- pb_plot_u20_all
u20_left[["layers"]][[1]][["aes_params"]]$size <- 1
u20_right[["layers"]][[3]][["aes_params"]]$size <- 1

((u20_left + u20_right) & 
    theme(legend.position = "bottom")) / 
  guide_area() + 
  plot_layout(guides = 'collect',
              width = 155,
              heights = c(80, 5)
              )

ggsave("graphs/u20_all.png", dpi = 400, width = 155, height = 85, units = "mm")

# joint graphs for poster of primary models -------------------------------

u18_left <- gr_u18_sp
u18_right <- pb_plot_u18_sp

u18_left[["layers"]][[1]][["aes_params"]]$size <- 1
u18_right[["layers"]][[3]][["aes_params"]]$size <- 1


((u18_left + u18_right) & 
    theme(legend.position = "bottom")) / 
  guide_area() + 
  plot_layout(guides = 'collect',
              width = 155,
              heights = c(80, 5),
              )

ggsave("graphs/u18_sp.svg", dpi = 400, width = 240, height = 90, units = "mm")

u20_left <- gr_u20_sp
u20_right <- pb_plot_u20_sp
u20_left[["layers"]][[1]][["aes_params"]]$size <- 1
u20_right[["layers"]][[3]][["aes_params"]]$size <- 1

((u20_left + u20_right) & 
    theme(legend.position = "bottom")) / 
  guide_area() + 
  plot_layout(guides = 'collect',
              width = 155,
              heights = c(80, 5)
              )

ggsave("graphs/u20_sp.svg", dpi = 400, width = 240, height = 90, units = "mm")

# comparing UK to other countries -----------------------------------------


rates_1990s <- synthData_u20_filt %>% 
  mutate(Country = ifelse(Country %in% c("England and Wales", "Scotland"), "UK", Country),
         weight = case_when(
           Country == "Scotland" ~ 5,
           Country == "England and Wales" ~ 65,
           TRUE ~ 1
         )) %>% 
  group_by(Country, Year) %>% 
  summarise(pRate= weighted.mean(pRate, w = weight)) %>% 
  filter(Year <= 2000) 

country_names <- rates_1990s %>% 
  filter(Year==2000)


rates_1990s %>% 
  ggplot(aes(Year, pRate, colour = fct_reorder2(factor(Country), pRate, Year == 2000, weighted.mean, .desc = TRUE))) +
  geom_line() +
  geom_line(size = 1) +
  theme_sphsu_light() +
  ylab(paste0("Under-20 pregnancy rate (per 1,000 women)")) +
  theme(panel.grid.major = element_line(colour = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,4,0,0), "cm"),
        text = element_text(family = "sans", size = 10)
  ) +
  scale_x_continuous(breaks = seq(1990, 2000, by = 2), expand = expansion(add = c(0, -1)), limits = c(1990, 2001)) +
  scale_colour_sphsu() +
  coord_cartesian(clip = "off") +
  geom_text_repel(data = country_names,
                  aes(x = Year+0.2, y = pRate, label = Country),
                  family = "sans",
                  hjust = 0,
                  nudge_x = 0.5,
                  size = 8*(5/14),
                  direction = "y",
                  xlim = c(NA, 2030),
                  ylim = c(-1, 75),
                  point.padding = 0)
  
ggsave("graphs/comparing_u20_uk.png", dpi = 600, width = 155, height  = 120, units = "mm")


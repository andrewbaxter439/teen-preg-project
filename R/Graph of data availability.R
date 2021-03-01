library(tidyverse)
library(readxl)
library(SPHSUgraphs)
library(patchwork)

its_sources <- read_excel("ITS_data_summ.xlsx")

supp_data <- tibble(Country = factor("Scotland", levels = c("England and Wales", "Scotland", "Wales", "England")),
                    Agegrp = factor(c("Under-18", "Under-20")),
                    First = 1987,
                    Last = 1994)

(preg_plot <- its_sources %>% 
  mutate(Agegrp = factor(Agegrp, levels = c("Under-16", "Under-18", "Under-20")),
         Country = factor(Country, levels = c("England and Wales", "Scotland", "Wales", "England"))) %>%
  filter(Stat == "Pregnancy rates") %>% 
  # ggplot(aes(x = Last, y = Agegrp, fill = Country)) +
  # geom_bar(stat = "identity", position = "dodge") 
  ggplot(aes(xmin = First, 
             xmax = Last,
             y = Country,
             ymin = as.numeric(Country)-0.3,
             ymax = as.numeric(Country) + 0.3,
             # ymin = as.numeric(interaction(Country, Agegrp))-0.3,
             # ymax = as.numeric(interaction(Country, Agegrp)) + 0.3,
             fill = Country)) +
    geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 1990, xmax = 2016),
              fill = "#ffeeaa",
              inherit.aes = FALSE) +
    geom_rect(data = tibble(Agegrp = "Under-18"),
              aes(ymin = -Inf, ymax = Inf, xmin = 1985, xmax = 2016.5),
              colour = "#808080",
              size = 1,
              fill = NA,
              inherit.aes = FALSE) +
  geom_rect() +
  facet_grid(Agegrp ~ .) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.key = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        panel.grid = element_blank()) +
  ggtitle("Reported pregnancy rates by country") +
  scale_fill_manual("", values = c("England" = sphsu_cols("Burgundy", names = FALSE),
                                   "England and Wales" = sphsu_cols("Leaf", names = FALSE), 
                                   "Scotland" = sphsu_cols("University Blue", names = FALSE), 
                                   "Wales" = sphsu_cols("Rust", names = FALSE)),
                    limits = c("England", "England and Wales", "Scotland", "Wales")) +
    coord_cartesian(xlim = c(1985, 2016)) +
    geom_text(data = tibble(Agegrp = "Under-16"),
              aes(x = 1990.5, y = Inf, label = "Target comparison\nperiod"),
              size = 3,
              vjust = 1.1, hjust = 0, inherit.aes = FALSE) +
    geom_rect(data = supp_data, fill = "#5588aa") +
    geom_text(data = supp_data,
              aes(x = First+0.5), 
              label = "Calculated rates",
              hjust = 0,
              size = 2,
              vjust = 0.5))
  
(add_plot <- its_sources %>% 
  filter(Stat != "Pregnancy rates") %>% 
  mutate(Stat = factor(Stat)) %>% 
  ggplot(aes(xmin = First,
             xmax = Last,
             y = Stat,
             ymin = as.numeric(Stat) - 0.3,
             ymax = as.numeric(Stat) + 0.3,
             fill = "Scotland")) + 
    geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 1990, xmax = 2016),
              fill = "#ffeeaa") +
  geom_rect() + 
  facet_grid(Agegrp ~ .) +
  ggtitle("Additional data used for Scottish rates") +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.key = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        panel.grid = element_blank()) +
  ylab("Statistic") +
  scale_fill_manual("", values = c("England" = sphsu_cols("Burgundy", names = FALSE),
                                   "England and Wales" = sphsu_cols("Leaf", names = FALSE), 
                                   "Scotland" = sphsu_cols("University Blue", names = FALSE), 
                                   "Wales" = sphsu_cols("Rust", names = FALSE)),
                    limits = c("England", "England and Wales", "Scotland", "Wales")))
  

preg_plot  / add_plot / 
  plot_layout(guides = "collect", heights = c(4.5, 2.5)) &
  theme(legend.position = "bottom",
        axis.title.x = element_blank())


# ggsave("Figure 3-2.svg", width = 15.5, height = 13, units = "cm", dpi = 300)
ggsave("graphs/Figure 3-2.png", width = 15.5, height = 13, units = "cm", dpi = 300)

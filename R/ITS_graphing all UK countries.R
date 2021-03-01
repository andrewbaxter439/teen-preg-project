library(patchwork)
library(tidyverse)
library(readxl)
library(export)

simple_theme <-   theme(
  panel.background = element_blank(),
  legend.key  = element_blank(),
  panel.grid.major = element_line(colour = "#e0e0e0"),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "#666666"),
  legend.text = element_text(size = 12)
)
theme_set(simple_theme)


UK_u16 <-  read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 16")
UK_u18 <-  read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 18")
UK_u20 <-  read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 20")


EstScot_u20 <- read_csv("EstScot_1985_2015.csv") %>% 
  mutate(Country = "Scotland") %>% 
  filter(Year>1986, Year < 1994)
EstScot_u18 <- read_csv("EstScotU18_1985_2015.csv") %>% 
  mutate(Country = "Scotland") %>% 
  filter(Year>1986, Year < 1994)

# England-only rates ----------------------------------------------------

UK_u18 %>% filter(Country == "England") %>%
  gather("Year", "Value",-1) %>%
  mutate(Year = as.numeric(Year)) %>% 
  # bind_rows(EstScot_u18 %>% select(Year, Country, Value)) %>% 
  filter(!is.na(Value), Year > 1991) %>% 
  ggplot(aes(
    x = Year,
    y = Value,
    col = Country
  )) +
  geom_rect(data = tibble(),
            aes(ymin = 0,
                ymax = Inf,
                xmin = 1999,
                xmax = 2005),
            fill = "grey",
            alpha = 0.3,
            inherit.aes = FALSE) +
  geom_rect(data = tibble(),
            aes(ymin = 0,
                ymax = Inf,
                xmin = 2005,
                xmax = 2010),
            fill = "grey",
            alpha = 0.5,
            inherit.aes = FALSE) +
  geom_point(shape = 3) +
  geom_line(size = 1) +
  ylab(paste0("Rate of pregnancies to under 18s, per 1,000")) +
  xlab("Year") +
  scale_colour_manual(
    limits = c("England", "Wales", "Scotland"),
    values = c("England" = "#CF142B",
               "Wales" = "#00AB39",
               "Scotland" = "#0072C6"),
    drop = FALSE) + 
  xlim(1990, 2016) +
  theme(legend.position = "none")  +
  scale_y_continuous(breaks = c(0, 20, 40),
                     limits = c(0, NA),
                     expand = expansion(add = c(0, 2))) +
  geom_text(
    data = tibble(
      label = c("1999 - Strategy implemented",
                "2005 - Mid-term review and step-up",
                "2010 - End of strategy"),
      Year = c(1999, 2005, 2010),
      vjust = c(-0.3, -0.3, 1.2),
      height = 2
    ),
    aes(x = Year, y = height, vjust = vjust, label = label),
    inherit.aes = FALSE,
    angle = 90,
    size = pts(14),
    hjust = 0)


ggsave("graphs/England rates 1992-2016.png", width = 155, height = 120, units = "mm", dpi= 300)

# All UK compare (figure 1-3) ---------------------------------------------

UK_u18 %>%
  gather("Year", "Value",-1) %>%
  mutate(Year = as.numeric(Year)) %>% 
  bind_rows(EstScot_u18 %>% select(Year, Country, Value)) %>%
  filter(!is.na(Value), Year > 1991) %>% 
  ggplot(aes(
    x = Year,
    y = Value,
    col = Country
  )) +
  geom_rect(data = tibble(),
            aes(ymin = 0,
                ymax = Inf,
                xmin = 1999,
                xmax = 2010),
            fill = "grey",
            alpha = 0.3,
            inherit.aes = FALSE) +
  # geom_rect(data = tibble(),
  #           aes(ymin = 0,
  #               ymax = Inf,
  #               xmin = 2005,
  #               xmax = 2010),
  #           fill = "grey",
  #           alpha = 0.5,
  #           inherit.aes = FALSE) +
  geom_point(shape = 3) +
  geom_line(size = 1) +
  ylab(paste0("Rate of pregnancies to under 18s, per 1,000")) +
  xlab("Year") +
  scale_colour_manual("",
    limits = c("England", "Wales", "Scotland"),
    values = c("England" = "#CF142B",
               "Wales" = "#00AB39",
               "Scotland" = "#0072C6"),
    drop = FALSE) + 
  xlim(1990, 2016) +
  scale_y_continuous(breaks = c(0, 20, 40),
                     limits = c(0, 60),
                     expand = expansion(add = c(0, 0))) +
  geom_text(
    data = tibble(
      label = c("Strategy period"),
      Year = c(2005),
      height = 10
    ),
  # geom_text(
  #   data = tibble(
  #     label = c("1999 - Strategy implemented",
  #               "2005 - Mid-term review and step-up",
  #               "2010 - End of strategy"),
  #     Year = c(1999, 2005, 2010),
  #     vjust = c(-0.3, -0.3, 1.2),
  #     height = 2
  #   ),
    aes(x = Year, y = height, label = label),
    inherit.aes = FALSE,
    size = pts(14),
    hjust = 0.5) +
  theme(legend.position = "bottom")

ggsave("graphs/Figure 1-3.png", width = 155, height = 120, units = "mm", dpi= 300)

# comparing all uk --------------------------------------------------------

theme_set(simple_theme + 
            theme(
              legend.position = "bottom",
              legend.key  = element_blank(),
              legend.key.width = unit(1, "cm")
            ))

(gr_u18_base <-
    UK_u18 %>% filter(Country == "Scotland" |
                        Country == "England" |
                        Country == "Wales") %>%
    gather("Year", "Value",-1) %>%
    mutate(Year = as.numeric(Year)) %>% 
    bind_rows(EstScot_u18 %>% select(Year, Country, Value)) %>% 
    filter(!is.na(Value), Year > 1991) %>% 
    ggplot(aes(
      x = Year,
      y = Value,
      col = Country
    )) +
    geom_point(shape = 3) +
    geom_line(size = 1) +
    ylab(paste0("Rate of pregnancies to under 18s, per 1,000")) +
    xlab("Year") +
    scale_colour_manual(
      limits = c("England", "Wales", "Scotland", "England and Wales"),
      # limits = c("England", "Wales", "Scotland"),
      values = c("England" = "#CF142B",
                 "Wales" = "#00AB39",
                 "England and Wales" = "#A50115",
                 "Scotland" = "#0072C6"
      ),
      drop = FALSE) + 
    xlim(1987, 2016) +
    theme(legend.position = "bottom")  +
  scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.03)))
  )

(gr_u18 <- gr_u18_base + ggtitle("a) Pregnancies to under-18s 1992-2016") +
    geom_vline(data = tibble(),
               xintercept = 1999,
               linetype = "dotted",
               col = "#000000CC") +
    geom_text(data = tibble(),
              aes(x =  2000, y = Inf, label = "1999 Strategy launch"), 
              col = "#666666",
              hjust = 0,
              vjust = 1.5,
              size = 5,
              inherit.aes = FALSE))

# ggsave("graphs/under-18 EWS 1992-2016.png", gr_u18, height = 120, width = 155, units = "mm", dpi = 400)

# graph2ppt(file = "graphs/under-18.pptx", height = 13.65/2.5, width = 17.37/2.5)

(gr_u18_EW <- UK_u18 %>% filter(Country == "Scotland" |
                                  Country == "England and Wales") %>%
    gather("Year", "Value",-1) %>%
    mutate(Year = as.numeric(Year)) %>%
    bind_rows(EstScot_u18 %>% select(Year, Country, Value)) %>% 
    filter(!is.na(Value)) %>% 
    ggplot(aes(
      x = Year,
      y = Value,
      col = Country
    )) +
    geom_point(shape = 3) +
    geom_line(size = 1) +
    ylab(paste0("Rate of pregnancies to Under 18s, per 1,000")) +
    xlab("Year") +
    scale_colour_manual(
      limits = c("England", "Wales", "Scotland", "England and Wales"),
      values = c("England" = "#CF142B",
                 "Wales" = "#00AB39",
                 "Scotland" = "#0072C6",
                 "England and Wales" = "#A50115"),
      drop = FALSE)  + 
    xlim(1987, 2016) + 
  scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.03))) +
    ggtitle("b) Pregnancies to under-18s 1987-2016 (England and Wales combined)")
  )

(gr_u16 <- UK_u16 %>% filter(Country == "Scotland" |
                               Country == "England and Wales") %>%
    gather("Year", "Value",-1) %>%
    filter(Year >= 1992) %>% 
    mutate(Year = as.numeric(Year)) %>%
    ggplot(aes(
      x = Year,
      y = Value,
      col = Country
    )) +
    geom_point(shape = 3) +
    geom_line(size = 1) +
    ylab(paste0("Rate of pregnancies to Under 16s, per 1,000")) +
    xlab("Year") +
    scale_colour_manual(
      limits = c("England", "Wales", "Scotland", "England and Wales"),
      values = c("England" = "#CF142B",
                 "Wales" = "#00AB39",
                 "Scotland" = "#0072C6",
                 "England and Wales" = "#A50115"),
      drop = FALSE) +
    xlim(1987, 2016) +
  scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.03))) +
    ggtitle("c) Pregnancies to under-16s 1992-2016 (England and Wales combined)")
  )

(gr_u20 <- UK_u20 %>% filter(Country == "Scotland" |
                               Country == "England and Wales") %>%
    gather("Year", "Value",-1) %>%
    mutate(Year = as.numeric(Year)) %>%
    bind_rows(EstScot_u20 %>% select(Year, Country, Value)) %>% 
    filter(!is.na(Value), Year > 1992) %>% 
    ggplot(aes(
      x = Year,
      y = Value,
      col = Country
    )) +
    geom_point(shape = 3) +
    geom_line(size = 1) +
    ylab(paste0("Rate of pregnancies to Under 20s, per 1,000")) +
    xlab("Year") +
    scale_colour_manual(
      limits = c("England", "Wales", "Scotland", "England and Wales"),
      values = c("England" = "#CF142B",
                 "Wales" = "#00AB39",
                 "Scotland" = "#0072C6",
                 "England and Wales" = "#A50115"),
      drop = FALSE) +
    xlim(1987, 2016) + 
  scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.03))) +
    ggtitle("d) Pregnancies to under-20s 1992-2016 (England and Wales combined)")
  )

(((gr_u18 | gr_u18_EW) /
    (gr_u16 | gr_u20   )) /
    guide_area() +
    plot_layout(guides = 'collect'#, widths = unit(173, "mm"), heights = unit(c(55,55,20), "mm"))
    ))

#%>% #ggsave("graphs/test2.svg",., units = "mm", height = 400, width = 400)
# graph2doc(file = "graphs/test2.docx", height = 13.65/2.5, width = 17.37/2.5)

gr_u18_base +
  scale_x_continuous()

ggsave("graphs/u18_UK_rates.svg", 
       gr_u18 +
         theme(
           # title = element_text(size = 14), 
           # axis.title = element_text(size = 12),
           #legend.position = "none",
           text = element_text(size = 12)
         ),
       dpi = 400,
       units = "mm", 
       width = 173,
       height = 136)

ggsave("graphs/u18_UK_rates_mono.svg", 
       gr_u18 +
         theme(
           # title = element_text(size = 14), 
           # axis.title = element_text(size = 12),
           #legend.position = "none",
           text = element_text(size = 12)
         ) +
         scale_colour_grey(limits = c("England", "Wales", "Scotland")),
       dpi = 400,
       units = "mm", 
       width = 173,
       height = 136)



ggsave("graphs/u18_UK_EW.svg", 
       gr_u18_EW +
         theme(title = element_text(size = 14), 
               axis.title = element_text(size = 12),
               legend.position = "none"),
       dpi = 400,
       units = "mm", 
       width = 200,
       height = 150)
ggsave("graphs/u16_UK_rates.svg", 
       gr_u16 +
         theme(title = element_text(size = 14), 
               axis.title = element_text(size = 12),
               legend.position = "none"),
       dpi = 400,
       units = "mm", 
       width = 200,
       height = 150)
ggsave("graphs/u20_UK_rates.svg", 
       gr_u20 +
         theme(title = element_text(size = 14), 
               axis.title = element_text(size = 12),
               legend.position = "none"),
       dpi = 400,
       units = "mm", 
       width = 200,
       height = 150)

graph2doc(gr_u18, "graphs/Under-18 EWS.docx", height = 13.65/2.5, width = 17.37/2.5)

# producing summaries -----------------------------------------------------

UK_u18 %>% group_by(Country) %>% select(`1998`, `2016`) %>%  mutate(drop = (`2016`-`1998`)/`1998`*100)
UK_u16 %>% group_by(Country) %>% select(`1998`, `2016`) %>%  mutate(drop = (`2016`-`1998`)/`1998`*100)
UK_u20 %>% group_by(Country) %>% select(`1998`, `2016`) %>%  mutate(drop = (`2016`-`1998`)/`1998`*100)

rates <- UK_u18 %>% filter(Country %in% c("England", "England and Wales")) %>% 
  gather("Year", "rate", -Country) %>% 
  filter(Year > 1991, Year < 2017) %>% 
  spread(Country, rate) %>% 
  select(Year, England, EW = `England and Wales`)


cor(rates$England, rates$EW)^2

rates %>% mutate(spe = (England - EW)^2) %>% 
  summarise(mspe = mean(spe),
            rmse = sqrt(mspe),
            R2 = cor(England, EW)^2)

Metrics::rmse(rates$England, rates$EW)

# output synth graphs -----------------------------------------------------

ulhg <- gr_u18_sp + 
  geom_line(data = tibble(x = 1999, y = 15), aes(x, y,  colour = "Placebo countries")) +
  scale_colour_manual(name = "Data",
                      breaks = c("Treated", "Synthetic", "Placebo countries"),
                      labels = c("England and Wales", "Synthetic", "Placebo countries"),
                      values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), 
                                 "Treated" = sphsu_cols("Thistle", names = FALSE),
                                 "Placebo countries" = "grey"), drop = FALSE) +
  theme(legend.position = "bottom",
        axis.line = element_blank(),
        legend.text = element_text(size=12)) +
  ggtitle("a) England and Wales vs Synthetic Control\nUnder-18 births")

urhg <- pb_plot_u18_sp +
  ggtitle("b) Gaps between observed and control, plotted by country\n")

ugs <- (ulhg | urhg)

llhg <- gr_u20_sp + 
  geom_line(data = tibble(x = 1999, y = 15), aes(x, y,  colour = "Placebo countries")) +
  scale_colour_manual(name = "Data",
                      breaks = c("Treated", "Synthetic", "Placebo countries"),
                      labels = c("England and Wales", "Synthetic", "Placebo countries"),
                      values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), 
                                 "Treated" = sphsu_cols("Thistle", names = FALSE),
                                 "Placebo countries" = "grey"), drop = FALSE) +
  theme(legend.position = "bottom",
        axis.line = element_blank(),
        legend.text = element_text(size=12)) +
  ggtitle("c) England and Wales vs Synthetic Control\nUnder-20 pregnancies")

lrhg <- pb_plot_u20_sp +
  ggtitle("d) Gaps between observed and control, plotted by country\n")

lgs <- (llhg | lrhg) 

(ugs / lgs) / guide_area() + plot_layout(guides = "collect", heights = c(6,6,1))


ggsave("graphs/Figure 4.svg", dpi = 300, units = "mm", width = 300, height = 300)


# start and end -----------------------------------------------------------

UK_u18 %>% filter(Country == "Scotland" |
                    Country == "England" |
                    Country == "Wales") %>%
  gather("Year", "Value",-1) %>%
  mutate(Year = as.numeric(Year)) %>% 
  bind_rows(EstScot_u18 %>% select(Year, Country, Value)) %>% 
  filter(!is.na(Value), Year %in% c(1998, 2016)) %>% 
  pivot_wider(names_from = "Year", values_from = "Value") %>% 
  mutate(perc_change = 100*(`1998`-`2016`)/`1998`)

synthData_u18_filt %>% 
  filter(Year < 1999) %>% 
  ggplot(aes(Year, rate, col = Country)) +
  geom_line(size = 1) +
  scale_colour_sphsu() +
  theme_sphsu_light() + 
  theme(
    panel.grid.major = element_line(colour = "#e0e0e0"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5,5,0,0), "cm"),
    text = element_text(family = "sans", size = 10)) +
  coord_cartesian(clip = "off") +
  geom_text_repel(data = synthData_u18_filt %>% 
                    filter(Year == 1998),
                  aes(x = Year+0.1,
                      y = rate,
                      label = Country),
                  family = "sans",
                  hjust = 0,
                  size = 8*(5/14),
                  nudge_x = 1,
                  direction = "y",
                  xlim = c(NA, 2005),
                  ylim = c(-1, 40),
                  point.padding = 0) +
  ylab("Under-18 birth rate (per 1,000 women)") +
  scale_x_continuous(expand = expansion(add = c(0, -1)), limits  = c(1990, 1999)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))

ggsave("graphs/Under-18 all countries.png", width = 155, height = 120, units = "mm", dpi = 400)





synthData_u20_filt %>% 
  filter(Year < 1999) %>% 
  ggplot(aes(Year, pRate, col = Country)) +
  geom_line(size = 1) +
  scale_colour_sphsu() +
  theme_sphsu_light() + 
  theme(
    panel.grid.major = element_line(colour = "#e0e0e0"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5,5,0,0), "cm"),
    text = element_text(family = "sans", size = 10)) +
  coord_cartesian(clip = "off") +
  geom_text_repel(data = synthData_u20_filt %>% 
                    filter(Year == 1998),
                  aes(x = Year+0.1,
                      y = pRate,
                      label = Country),
                  family = "sans",
                  hjust = 0,
                  size = 8*(5/14),
                  nudge_x = 1,
                  direction = "y",
                  xlim = c(NA, 2005),
                  ylim = c(-1, 100),
                  point.padding = 0) +
  ylab("Under-20 pregnancy rate (per 1,000 women)") +
  scale_x_continuous(expand = expansion(add = c(0, -1)), limits  = c(1990, 1999)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))

ggsave("graphs/Under-20 all countries.png", width = 155, height = 120, units = "mm", dpi = 400)


# save across all years ---------------------------------------------------

synthData_u18_filt %>% 
  ggplot(aes(Year, rate, col = Country)) +
  geom_line(size = 1) +
  scale_colour_sphsu() +
  theme_sphsu_light() + 
  theme(
    panel.grid.major = element_line(colour = "#e0e0e0"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5,5,0,0), "cm"),
    text = element_text(family = "sans", size = 10)) +
  coord_cartesian(clip = "off") +
  geom_text_repel(data = synthData_u18_filt %>% 
                    filter(Year == 2013),
                  aes(x = Year+0.1,
                      y = rate,
                      label = Country),
                  family = "sans",
                  hjust = 0,
                  size = 8*(5/14),
                  nudge_x = 2.5,
                  direction = "y",
                  xlim = c(NA, 2025),
                  ylim = c(-3, 40),
                  point.padding = 0) +
  ylab("Under-18 birth rate (per 1,000 women)") +
  scale_x_continuous(expand = expansion(add = c(0, -1)), limits  = c(1990, 2014)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))

ggsave("graphs/Under-18 all countries_all years.png", width = 155, height = 120, units = "mm", dpi = 400)





synthData_u20_filt %>% 
  ggplot(aes(Year, pRate, col = Country)) +
  geom_line(size = 1) +
  scale_colour_sphsu() +
  theme_sphsu_light() + 
  theme(
    panel.grid.major = element_line(colour = "#e0e0e0"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5,5,0,0), "cm"),
    text = element_text(family = "sans", size = 10)) +
  coord_cartesian(clip = "off") +
  geom_text_repel(data = synthData_u20_filt %>% 
                    filter(Year == 2013),
                  aes(x = Year+0.1,
                      y = pRate,
                      label = Country),
                  family = "sans",
                  hjust = 0,
                  size = 8*(5/14),
                  nudge_x = 2.5,
                  direction = "y",
                  xlim = c(NA, 2025),
                  ylim = c(-1, 100),
                  point.padding = 0) +
  ylab("Under-20 pregnancy rate (per 1,000 women)") +
  scale_x_continuous(expand = expansion(add = c(0, -1)), limits  = c(1990, 2014)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))

ggsave("graphs/Under-20 all countries_all years.png", width = 155, height = 120, units = "mm", dpi = 400)

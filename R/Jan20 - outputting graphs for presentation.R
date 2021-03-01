######################################################
#                                                    #
#  Calculating from synth-report-updated.rmd outputs #
#                                                    #
######################################################


library(patchwork)
library(export)


gr_u18_rates + 
  scale_y_continuous(expand = c(0,0), limits = c(0,42)) +
  theme(axis.line.x = element_line(size = 1)) +
  scale_colour_manual(breaks = new_labs$label, values = new_labs$colour) +
  gr_u18_sp +
  scale_y_continuous(expand = c(0,0), limits = c(0,42)) +
  theme(axis.line.x = element_line(size = 1),
        legend.position = "none")

ggsave("startergraphs.svg", units = "mm", height = 150, width = 300)

pb_plot_u18_sp + pp_u18_sp
graph2ppt(last_plot(), "graphs/presentation_graphs.pptx", height = 20/2.5, width = 35/2.5, append = TRUE)

gr_u20_rates
graph2ppt(last_plot(), "graphs/presentation_graphs.pptx", height = 20/2.5, width = 20/2.5, append = TRUE)

gr_u20_sp
graph2ppt(last_plot(), "graphs/presentation_graphs.pptx", height = 18/2.5, width = 18/2.5, append = TRUE)


pb_plot_u20_sp + pp_u20_sp
graph2ppt(last_plot(), "graphs/presentation_graphs.pptx", height = 20/2.5, width = 35/2.5, append = TRUE)


md_u20_sp %>% 
  filter(Year < 1999) %>% 
  summarise(mean= mean(Rate))

(gr_u18_ns + theme(legend.position = 'bottom') + gr_u20_ns + theme(legend.position = 'bottom')) /
  guide_area() +
  plot_layout(guides = 'collect', heights = c(9,1))
graph2ppt(last_plot(), "graphs/presentation_graphs.pptx", height = 12/2.5, width = 29.17/2.5, append = TRUE)

pb_plot_u18_ns+ pb_plot_u20_ns

graph2ppt(last_plot(), "graphs/presentation_graphs.pptx", height = 10.9/2.5, width = 29.17/2.5, append = TRUE)



# all pre-rates -----------------------------------------------------------

labels <- synthData_u18_filt %>% 
  filter(Year==1998) %>% 
  mutate(Country = ifelse(Country == "United States of America", "U.S.A.", Country))

(pres_plot <-
    synthData_u18_filt %>% 
    filter(Year<1999) %>% 
    mutate(linesize = ifelse(Country == "England and Wales", "E", "O"),
           Country = ifelse(Country == "United States of America", "U.S.A.", Country)) %>% 
    ggplot(aes(Year, rate, col = Country)) +
    geom_line(aes(size = linesize)) +
    scale_size_manual(values = c(1.8, 1)) +
    # scale_colour_sphsu() +
    scale_colour_manual(breaks = new_labs$label, values = new_labs$colour) +
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          axis.line = element_line(size = 0.1, colour = "grey"),
          axis.line.y = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(0,7,2,0), "cm"),
          text = element_text(family = "sans")) +
    coord_cartesian(clip = "off") +
    geom_text_repel(data = labels,
                    aes(x = Year+0.1, y = rate, label = Country),
                    family = "sans",
                    hjust = 0,
                    nudge_x = 0.9,
                    direction = "y",
                    size = 14*0.35,
                    xlim = c(NA, 2002),
                    ylim = c(-1, 75),
                    point.padding = 0) +
    ylab("Under-20 pregnancy rate (per 1,000 women)") +
    scale_x_continuous(expand = expand_scale(add = c(0, -1)), limits  = c(1990, 1999)) +
    scale_y_continuous(expand = c(0,0))
    )
# graph2ppt(last_plot(), "graphs/presentation_graphs.pptx", height = 20/2.5, width = 35/2.5, append = TRUE)

ggsave("graphs/Under-18 all countries.svg", pres_plot, width = 200, height = 150, units = "mm")


# setting colors manually -------------------------------------------------

gg_dat <- ggplot_build(pres_plot)$data[[2]]

new_labs <- gg_dat %>% 
  select(colour, label) %>% 
  mutate(colour = case_when(label == "England and Wales" ~ "#8B196E", 
                         label == "Scotland" ~ "#005981",
                         TRUE ~ colour))


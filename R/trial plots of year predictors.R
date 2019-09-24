load("Data/synth_data.rdata")
load("Data/iterations.rdata")
source("R/Synth_functions.R")
# plot function ----------------------------------------------------------------------------------------------

plotIterations <- function(iteration = it_u18_rateSp, labels = FALSE) {
  
  require(dplyr)
  require(ggplot2)
  require(SPHSUgraphs)
  require(purrr)
  require(ggpubr)
  require(tidyr)
  
#   mspes <- iteration %>% 
#   mutate(groups = as.factor(groups)) %>% 
#   ggplot(aes(mspe, fill = groups)) +
#   geom_histogram(bins = 50) +
#   theme_sphsu_light() +
#   scale_fill_sphsu()
#   
#   gaps <- iteration %>% 
#   select(gaps) %>%
#   map(bind_rows) %>%
#   pluck("gaps") %>% 
#   mutate(groups = as.factor(groups)) %>% 
#   ggplot(aes(Year, Gap, group = iteration, col = groups)) + 
#   geom_segment(x = 1985, xend = 2013, y = 0, yend = 0, col = "black") + 
#   geom_line(size = 1, alpha = 0.8) +
#   theme_minimal()+
#   theme(panel.grid = element_blank())+
#   geom_vline(xintercept = 1998.5, linetype = "dashed", col = "grey") +
#   scale_colour_sphsu()
# 
# ggarrange(mspes, gaps, ncol = 2, common.legend = TRUE, legend = "right")
#   
# }

# find top countries -----------------------------------------------------------------------------------------

weight_labels <- it_u18_rateSp %>% 
  select(iteration, w_weights, mspe) %>% 
  unnest() %>% 
  group_by(iteration) %>% 
  top_n(1,w.weights) %>% 
  ungroup() %>% 
  group_by(unit.names) %>% 
  top_n(1, -mspe) %>% 
  mutate(weight = paste0(w.weights*100, "%"),
         label = paste0(unit.names, ", ", weight, ", ", "MSPE = ", round(mspe, 3)))

label_pos <- it_u18_rateSp %>%
  select(gaps) %>%
  map(bind_rows) %>%
  pluck("gaps") %>% 
  filter(Year<=1998) %>%
  mutate(groups = as.factor(groups)) %>% 
  inner_join(weight_labels, by = "iteration") %>% 
  mutate(label = ifelse(Year == 1998, label, NA))

# Under-18 special predictors --------------------------------------------------------------------------------


mspes <-
  it_u18_rateSp %>% 
  mutate(groups = as.factor(groups)) %>% 
  ggplot(aes(mspe, fill = groups)) +
  geom_histogram(bins = 100) +
  theme_sphsu_light() +
  scale_fill_sphsu() 

if(labels) {
  mspes <- mspes +
  geom_text(data = label_pos, aes(x = mspe, y = 40, label = label),
            hjust = 0,
            angle = 45,
            inherit.aes = FALSE)
}


gaps <- it_u18_rateSp %>%
  select(gaps) %>%
  map(bind_rows) %>%
  pluck("gaps") %>% 
  mutate(groups = as.factor(groups)) %>% 
  filter(Year<1999) %>% 
  ggplot(aes(Year, Gap, col = groups, group = iteration)) + 
  geom_line(size = 1, alpha = 0.2) +
  geom_line(data = label_pos, alpha = 1, size = 2) +
  geom_segment(x = 1985, xend = 1999, y = 0, yend = 0, col = "black") + 
  theme_minimal()+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 1998.5, linetype = "dashed", col = "grey") +
  scale_colour_sphsu()

if(labels){
  gaps <- gaps +
  geom_text_repel(data = label_pos %>% filter(Year == 1998), aes(x = 1998, y = Gap, label = unit.names),
            hjust = 0,
            direction = "y",
            nudge_x = 0.75,
            xlim = c(1985, 2010),
            inherit.aes = FALSE,
            na.rm = TRUE) +
  theme(plot.margin = unit(c(0,5,0,0), "cm")) +
  coord_cartesian(clip = 'off')
}

ggarrange(mspes, gaps, ncol = 2, common.legend = TRUE, legend = "bottom")

}
ggsave("graphs/Under 18 pregnancies - testing iterations.png", height = 200, width = 400, units = "mm")



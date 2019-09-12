library(gganimate)
library(SPHSUgraphs)
library(plotly)
library(dplyr)
library(Synth)
library(tidyr)
`-.gg` <- function(e1, e2) e2(e1)


# Under-18 - special predictors ------------------------------------------------------------------------------


super_md_u18_sp <- tibble()

for (y in 1995:2005){
  

dp <- dataprep(
  foo = synthData_U18 %>% filter(Year< 2014),
  special.predictors = list(
    list("rate", 1985:1987, "mean"),
    list("rate", 1988:1990, "mean"),
    list("rate", 1991:1993, "mean"),
    list("rate", 1994:y, "mean")
  ),
  predictors.op = "mean",
  time.predictors.prior = 1985:y,
  dependent = "rate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls,
  time.optimize.ssr = 1990:y,
  time.plot = 1985:2013
)
  md <- predvalues_synth(dp, synth_outputs = FALSE, yr = y) %>% 
  mutate(IntYr = y,
         mspe = pre_MSPE(md))
  super_md_u18_sp <- bind_rows(super_md_u18_sp, md)
}

# super_md_u18_sp <- super_md_u18_sp %>% mutate(IntYr = sort(rep(seq(1995, 2005), 58))) 
# super_md_u18_sp %>% 
# gg_synth(md = .) + transition_states(states = super_md_u18_sp$IntYr, transition_length = 1, state_length = 2)

tp_u18_sp <- super_md_u18_sp %>% 
ggplot(aes(Year, Rate, col = Group, linetype = Group)) +
  geom_line(size = 2) +
  theme_sphsu_light() +
  ylab(paste0("Under-18 birth rate (per 1,000 women)")) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        line = element_blank()) +
  scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
  scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
  # geom_vline(xintercept = IntYr-0.5, linetype = "dotted", frame = IntYr) +
  geom_segment(aes(x = IntYr, xend = IntYr, y = 0, yend = 20), linetype = "dotted", size = 1, col = "#00000066") +
  transition_states(IntYr, transition_length = 1, state_length = 2) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(title = paste0("Year: {closest_state}")) +
  theme(text = element_text(size = 18),
        plot.title = element_text(size = 24, face = "bold")) +
  ease_aes('sine-in-out')

options(gganimate.dev_args = list(width = 1024, height = 768))

animate(tp_u18_sp)
       

# plotly style -----------------------------------------------------------------------------------------------

tp_u18_sp_plotly <- super_md_u18_sp %>% 
  ggplot(aes(Year, Rate, col = Group, linetype = Group, frame = IntYr)) +
  geom_line(size = 1) +
  theme_sphsu_light() +
  ylab(paste0("Under-18 birth rate (per 1,000 women)")) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        line = element_blank()) +
  scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
  scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
  geom_segment(aes(x = IntYr, xend = IntYr, y = 0, yend = 20), linetype = "dotted", size = 1, col = "#000000", alpha = 0.7) +
  scale_y_continuous(limits = c(0, 20)) - ggplotly

tp_u18_sp_plotly %>% 
  animation_slider(currentvalue = list(prefix = "Year ", font = list(color="black"))) %>% 
  animation_opts(easing = 'sin-in-out') %>% 
  api_create(filename = "England under-18 - special predictors - placebo time")



# Under-20 - special predictors ------------------------------------------------------------------------------

super_md_u20_sp <- tibble()

for (y in 1995:2005) {
  sp <- ifelse(y >= 1997, 
               (list(list("pRate", 1990:1996, "mean"),
               list("pRate", 1997:y, "mean"))),
               list(list("pRate", 1990:y, "mean"))
  )
  if (y >= 1997){
    sp <- list(list("pRate", 1990:1996, "mean"),
               list("pRate", 1997:y, "mean"))
  } else {
    sp <- list(list("pRate", 1990:y, "mean"))
  }
  # sp <- ifelse(y >= 1997, 
  #              (list(list("pRate", 1990:1996, "mean"),
  #              list("pRate", 1997:y, "mean"))),
  #              list(list("pRate", 1990:y, "mean"))
  # )
  
dp <- dataprep(
  foo = synthData_u20,
  special.predictors = sp,
  predictors.op = "mean",
  time.predictors.prior = 1990:y,
  dependent = "pRate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = u_20_codes$Code[u_20_codes$Country =="England and Wales"],
  controls.identifier = u_20_codes$Code[u_20_codes$Country !="England and Wales"],
  time.optimize.ssr = 1994:y,
  time.plot = 1990:2013
)
  md <- predvalues_synth(dp, synth_outputs = FALSE, yr = y) %>% 
  mutate(IntYr = y,
         mspe = pre_MSPE(md))
  super_md_u20_sp <- bind_rows(super_md_u20_sp, md)
}

tp_u20_sp_plotly <- super_md_u20_sp %>% 
ggplot(aes(Year, Rate, col = Group, linetype = Group, frame = IntYr)) +
  geom_line(size = 1) +
  theme_sphsu_light() +
  ylab(paste0("Under-20 pregnancy rate (per 1,000 women)")) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        line = element_blank()) +
  scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
  scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
  geom_segment(aes(x = IntYr, xend = IntYr, y = 0, yend = 80), linetype = "dotted", size = 1, col = "#000000", alpha = 0.7) +
  scale_y_continuous(limits = c(0, 80)) - ggplotly

tp_u20_sp_plotly %>% 
  animation_slider(currentvalue = list(prefix = "Year ", font = list(color="black"))) %>% 
  animation_opts(easing = 'sin-in-out')

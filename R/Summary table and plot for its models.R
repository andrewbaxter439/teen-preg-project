library(tidyverse)
library(meta)
library(patchwork)
library(gridExtra)

source('R/read_in_its_results.R', echo=FALSE)


meta_analyses %>%
  map(
    ~ forest(
      .x$re,
      prediction = FALSE,
      comb.fixed = FALSE,
      leftlabs = c("Model", str_to_sentence(.x$Coefficient), "SE"),
      print.subgroup.labels = TRUE,
      overall = TRUE
    )
  )

simple_theme <-   theme(
  panel.background = element_blank(),
  legend.key  = element_blank(),
  panel.grid.major = element_line(colour = "#e0e0e0"),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "#666666"),
  legend.text = element_text(size = 12)
)
theme_set(simple_theme)


tidied_results <- results %>%
  mutate(Coefficient = factor(Coefficient, levels = c("trend", "level"),
                              labels = str_to_sentence(c("trend", "level"))),
         facet = factor(case_when(
           comparison == 0 ~ "England only models",
           primary == "Primary comparison" ~ "Comparison models",
           TRUE ~ primary
         ), 
         levels = c("England only models", "Comparison models", "Robustness and sensitivity")),
         model = fct_relabel(model, str_to_sentence)) 

# Eng_only <- tidied_results %>%
#   filter(comparison == 0) %>% 
#   ggplot(aes(Estimate, fct_rev(model))) +
#   geom_vline(xintercept = 0, colour = "darkgrey", size = 1) +
#   geom_point(size = 3, shape = 'diamond') +
#   geom_linerange(aes(xmin = `Lower 95% CI`, xmax = `Upper 95% CI`)) +
#   theme(panel.grid.major.y = element_blank(),
#         axis.line.y = element_blank(),
#         panel.spacing = unit(2, "cm"),
#         plot.margin = unit(c(0.5,2.5,0.5,0.5), units = "cm")) +
#   # scale_x_continuous(expand = expansion(add = c(0.5, 0.5))) +
#   scale_y_discrete("Model") +
#   facet_grid(. ~ Coefficient,
#              scales = "free") +
#   geom_text(aes(x = Inf, 
#                 label = paste0(Estimate, "\n(", `Lower 95% CI`, " to ", `Upper 95% CI`, ")")),
#             hjust = 0) +
#   coord_cartesian(clip = "off") +
#   ggtitle("Coefficients of England-only models")


all_plot <- tidied_results %>%
  # filter(comparison == 1) %>% 
  ggplot(aes(Estimate, fct_rev(model))) +
  geom_vline(xintercept = 0, colour = "darkgrey", size = 1) +
  geom_point(size = 3, shape = 'diamond') +
  geom_linerange(aes(xmin = `Lower 95% CI`, xmax = `Upper 95% CI`)) +
  theme(panel.grid.major.y = element_blank(),
        axis.line.y = element_blank(),
        panel.spacing.x = unit(3.5, units = "cm"),
        panel.spacing.y = unit(1, "cm"),
        plot.margin = unit(c(0.5,3.5,0.5,0.5), units = "cm"),
        strip.background.y = element_blank(),
        strip.placement = "outside") +
  scale_y_discrete("Model") +
  facet_grid(facet ~ Coefficient,
             scales = "free",
             space = "free_y",
             switch = "y") +
  geom_text(aes(x = Inf, 
                label = paste0(round(digits = 2,Estimate), " (",
                               round(digits = 2,`Lower 95% CI`), " to ", 
                               round(digits = 2,`Upper 95% CI`), ")")),
            hjust = 0) +
  coord_cartesian(clip = "off")


pg <- ggplotGrob(all_plot)

pg$grobs[[21]]$layout$clip <- "off"

ggpubr::as_ggplot(pg)

ggsave(
  "graphs/Figure 5-13.png",
  width = 155,
  height = 160,
  units = "mm",
  dpi = 300
)


# Try it as a table -------------------------------------------------------

all_tidy_df <- tidied_results %>%
  mutate(
    Comparators = case_when(
      model %in% c("Model 1", "Model 2", "Model 5") ~ "England only",
      model %in% c("Model 3", "Model 6") ~ "England and Scotland",
      model %in% c("Model 4", "Model 7") ~ "England and Wales",
      model %in% c("Model 8", "Model 9", "Model 10") ~ "Eng/Wal and Scotland",
    ),
    `Age group` = case_when(
      model == "Model 9" ~ "Under 16",
      model == "Model 10" ~ "Under 20",
      TRUE ~ "Under 18"
    ),
    `Pill scare` = case_when(
      model == "Model 1" ~ "",
      TRUE ~ "\u2713"
    ),
    `Common shock` = case_when(
      model %in% c("Model 5", "Model 6", "Model 7") ~ "\u2713",
      TRUE ~ ""
    ),
    est = paste0(round(digits = 2,Estimate), " (",
                 round(digits = 2,`Lower 95% CI`), " to ", 
                 round(digits = 2,`Upper 95% CI`), ")"),
    model = fct_rev(model)
  )

draw_forest_table <- function(coefficient = "Trend", data = all_tidy_df) {
  

forest1 <- all_tidy_df %>% 
  filter(Coefficient == coefficient) %>% 
  ggplot(aes(Estimate, model)) +
  geom_vline(xintercept = 0, colour = "darkgrey", size = 1) +
  geom_point(size = 3, shape = 'diamond') +
  geom_linerange(aes(xmin = `Lower 95% CI`, xmax = `Upper 95% CI`)) +
  theme(panel.grid.major.y = element_blank(),
        panel.spacing.y = unit(1, "cm"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(), 
        panel.background=element_rect(colour = "lightblue"),
        strip.background.y = element_blank(),
        plot.margin = margin(l = 0, r = 0),
        strip.text = element_blank(),
        plot.title = element_text(size = 10),
        strip.placement = "outside") +
  facet_grid(facet ~ .,
             scales = "free",
             space = "free_y",
             switch = "y") +
  ggtitle(" \n ")

base_plot <- all_tidy_df %>% 
  filter(Coefficient == coefficient) %>% 
  ggplot(aes(x = 0, y = model)) +
  ylab(NULL) +
  xlab(" ") +
  facet_grid(facet ~ .,
             scales = "free",
             space = "free_y",
             switch = "y") +
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(1, "cm"),
        plot.margin = margin(l = 0, r = 0),
        axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_line(colour = "white"),
        axis.line.x=element_line(colour = "white"),#
        axis.text.x=element_text(color="white"),
        ## need text to be printed so it stays aligned with figure but white so it's invisible
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_rect(colour = "lightblue"),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        plot.title = element_text(size = 10, hjust = 0)) +
  coord_cartesian(clip = "off") +
    scale_x_continuous(expand = expansion(add = c(0,0)), limits = c(0,2))


lab1 <- base_plot + 
  geom_text(aes(label = Comparators), hjust = 0, size = pts(9)) +
  # xlim(0, 2) +
  ggtitle("\nComparators") +
  theme(strip.text = element_text(colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside" )


lab2 <- base_plot + 
  geom_text(aes(label = `Age group`), hjust = 0, size = pts(9)) +
  ggtitle("\nAge group")

lab3 <- base_plot + 
  geom_text(aes(x = 0, label = `Pill scare`), hjust = 0.5, size = pts(9)) +
  ggtitle("Pill\nscare") +
    scale_x_continuous(expand = expansion(add = c(0,0)), limits = c(-1,1)) +
  theme(plot.title = element_text(hjust = 0.5))

lab4 <- base_plot + 
  geom_text(aes(x = 0, label = `Common shock`), hjust = 0.5, size = pts(9)) +
  ggtitle("2008\nshock") +
    scale_x_continuous(expand = expansion(add = c(0,0)), limits = c(-1,1)) +
  theme(plot.title = element_text(hjust = 0.5))

lab5 <- base_plot + 
  geom_text(aes(label = est, x = Inf), hjust = 1, size = pts(9)) +
  ggtitle(glue::glue("{coefficient} change\n(95%CI)")) +
  theme(plot.title = element_text(hjust = 0.5))

lab1_grob <- ggplotGrob(lab1)
for (i in grep("strip", lab1_grob$grobs)) {
  lab1_grob$grobs[[i]]$layout$clip <- "off"
}

grid.arrange(lab1_grob, lab2, lab3, lab4, lab5, forest1, layout_matrix = 
               matrix(c(
                 1,1,1,
                 1,1,1,1,1,
                        2,2,
                        2,2,
                        3,
                        3,
                        4,
                        4,
                        5,5,5,
                        5,5,5,5,
                        6,6,6,6,
                        6,6,6,6
                 ), nrow = 1))


}

trend_forest <- draw_forest_table()

ggsave(
  "graphs/Figure 5-13_alt.png",
  trend_forest,
  width = 155,
  height = 160,
  units = "mm",
  dpi = 400
)

level_forest <- draw_forest_table("Level")

ggsave(
  "graphs/Figure 5-14_alt.png",
  level_forest,
  width = 155,
  height = 160,
  units = "mm",
  dpi = 400
)


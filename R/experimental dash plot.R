# pp_u18_sp <- gg_pre_postMSPE(md_u18_sp, pl_u18_filt) +
# scale_x_log10(labels = scales::comma_format(accuracy = 1), breaks = c(0,1,10, 100, 1000)) + scale_y_continuous(breaks = c(0,1,2,3)) + theme(plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))


library(gridExtra)

df <- md_u18_sp %>% 
  spread(Group, Rate) %>% 
  mutate(Gap = Treated - Synthetic,
         Country = "England and Wales") %>% 
  select(Year, Country, Gap) %>% 
  bind_rows(pl_u18_filt %>% select(Year, Country, Gap)) %>% 
  mutate(period = ifelse(Year<1999, "pre", "post")) %>% 
  group_by(Country, period) %>% 
  summarise(mspe = mean(Gap**2)) %>% 
  spread(period, mspe) %>% 
  mutate(ratio = post/pre,
         label = ifelse(Country=="England and Wales", paste0("England and Wales\nratio = ", signif(ratio, 3)), NA),
         xintercept = ifelse(Country=="England and Wales", ratio, NA))

library(scales)
reverselog_trans <- function(base = 10) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

ggplot(df, aes(0, ratio)) +
  geom_segment(aes(xend = -1, yend = ratio, colour = label), 
               size = ifelse(is.na(df$label), 0.5, 2)
               ) +
  theme_minimal() + 
  theme(panel.grid = element_blank()) +
  scale_y_continuous("Countries ranked by Pre/post MSPE ratio", trans = "log10")+ 
  geom_text(aes(x = 0.2, y = ratio,  label = label),
                vjust = 0.5, hjust = 0, inherit.aes = FALSE,
                nudge_x = 0.05) +
  coord_cartesian(clip = "off") +
  theme(
    plot.title = element_text(margin = margin(b = 1, unit = "cm")),
    plot.margin = margin(0, unit = "cm"),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.line.y = element_line(colour = "grey"),
    axis.title.x = element_blank()
  ) +
  scale_x_continuous("", limits = c(-1, 5), expand = expansion(add = 0))

ggsave("test_plot_1.png", dpi = 300, height = 12, width = 6, units = "cm")



# doing graph as in Abadie 2015 -------------------------------------------



df <- md_u18_sp %>% 
  spread(Group, Rate) %>% 
  mutate(Gap = Treated - Synthetic,
         Country = "England and Wales") %>% 
  select(Year, Country, Gap) %>% 
  bind_rows(pl_u18_filt %>% select(Year, Country, Gap)) %>% 
  mutate(period = ifelse(Year<1999, "pre", "post")) %>% 
  group_by(Country, period) %>% 
  summarise(mspe = mean(Gap**2)) %>% 
  spread(period, mspe) %>% 
  mutate(ratio = post/pre)

df_tidy <- df %>% 
  ungroup() %>% 
  mutate(rank = rank(-ratio),
         Country = fct_reorder(factor(Country), ratio),
         ratio_lab = str_pad(str_replace(as.character(round(ratio, 2)), "(\\.\\d)$", "\\10"), 6, side = "left"),
         EngWa = ifelse(Country == "England and Wales", "E", "C")) %>% 
  arrange(rank) %>% 
  select(Country, ratio, ratio_lab, rank, EngWa)
  

points_plot <- ggplot(df_tidy, aes(ratio, Country, colour = EngWa)) +
  geom_point(size = 3, shape = 'diamond') +
  scale_x_continuous("Post/pre-MSPE ratio") +
  ggtitle(" ") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
panel.background = element_blank(),
panel.grid.major = element_line(colour = "#e0e0e0"),
panel.grid.minor = element_blank(),
axis.line = element_line(colour = "#666666"),
    strip.text = element_blank(),
    panel.spacing.y = unit(1, "cm"),
    plot.margin = margin(l = 0, r = 0),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_colour_manual(values = c("E" = sphsu_cols("University blue", names = FALSE), "C" = "darkgrey"))

base_plot <- df_tidy %>% 
  ggplot(aes(x = 0, y = Country, colour = EngWa, fontface = ifelse(EngWa == "E", "bold", "plain"))) +
  ylab(NULL) +
  xlab(" ") +
  theme(
    strip.text = element_blank(),
    plot.margin = margin(l = 0, r = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color = "white"),
panel.background = element_blank(),
    ## need text to be printed so it stays aligned with figure but white so it's invisible
    legend.position = "none",
    # panel.background = element_rect(colour = "lightblue", fill = "white"),
    # panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12, hjust = 0)
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(add = c(0,0)), limits = c(0,2)) +
  scale_colour_manual(values = c("E" = sphsu_cols("University blue", names = FALSE), "C" = "black"))

c_plot <- base_plot + 
  geom_text(aes(label = Country), hjust = 0, size = pts(9)) +
  ggtitle("Country") +
  theme(strip.text = element_text(colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside" )


rank_plot <- base_plot + 
  geom_text(aes(label = rank), hjust = 0, size = pts(9)) +
  ggtitle("Rank") +
  theme(strip.text = element_text(colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside" )

rat_plot <- base_plot + 
  geom_text(aes(x = Inf, label = ratio_lab), hjust = 1, size = pts(9)) +
  ggtitle("Ratio") +
  theme(strip.text = element_text(colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(size = 12, hjust = 1))

ratio_tab_plot <- grid.arrange(c_plot, rank_plot, rat_plot, points_plot,
             layout_matrix = matrix(c(1,1,1,1,1,1,2, 2,3,3, 4, 4,4,4,4,4,4,4,4,4,4,4), nrow = 1))

ggsave("graphs/test_ratio_plot.png", ratio_tab_plot, dpi = 400,   width = 155,
       height = 120,
       units = "mm")

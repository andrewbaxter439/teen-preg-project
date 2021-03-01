library(tidyverse)
library(SPHSUgraphs)

relative_changes <- read_csv("Data/relative_changes_2017.csv", skip=6)

relative_changes %>% 
  rename(Year = X1) %>% 
  pivot_longer(-Year, 
               names_to="agegrp",
               values_to="relrate", 
               names_transform=list(agegrp = as.factor)) %>% 
  mutate(agegrp = fct_reorder2(agegrp, Year, relrate, last2),
         linewidth = ifelse(agegrp == "All ages", 2, 1)) %>% 
  ggplot(aes(x=Year, y = relrate, colour=agegrp, size = linewidth)) +
  geom_hline(yintercept = 100) +
  geom_line() +
  theme_sphsu_light() +
  scale_colour_sphsu(name = "Age group") +
  scale_size_continuous(range = c(1,2), guide = "none") +
  ylab("Pregnancy rate relative to 1990 (%)") +
  theme(text = element_text(size = 14))


ggsave("graphs/Relative rates ENGWASCO.png", units = "mm", dpi=300, width = 210, height = 130)

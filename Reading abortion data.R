library(tidyverse)

# European Health Information Gateway abortion stats

all.Europe.ab <- read_csv("EHIG_abortions.csv", skip=25)[1:1208,]
all_EU_ab_tidy <- all.Europe.ab %>%
  select(Code=COUNTRY, Year=YEAR, Total=VALUE) %>%
  filter(!is.na(Code)) %>%
  left_join(ccodes, by="Code") %>%
  mutate(Country = ifelse(Code=="FRA", "France",
                          ifelse(Code=="GBR", "United Kingdom",
                                 ifelse(Code=="DEU", "Germany", Country)))) %>%
  select(Country, Year, Total, Code)

country_names <-
  c(
    "Austria",
    "Australia",
    "Belgium",
    "Bulgaria",
    "Canada",
    "Croatia",
    "Cyprus",
    "Czechia",
    "Denmark",
    "Estonia",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Iceland",
    "Ireland",
    "Italy",
    "Latvia",
    "Lithuania",
    "Luxembourg",
    "Malta",
    "Netherlands",
    "New Zealand",
    "Norway",
    "Poland",
    "Portugal",
    "Romania",
    "Slovak Republic",
    "Slovenia",
    "Spain",
    "Sweden",
    "Switzerland",
    "United Kingdom",
    "Scotland",
    "England and Wales",
    "Northern Ireland",
    "United States of America"
  )




country_names %>% subset(!(country_names %in% all_EU_ab_tidy$Country)) # countries with missing data
all_EU_ab_tidy$Country %>% subset(!(all_EU_ab_tidy$Country %in% country_names)) %>% unique() # countries included in database, not used here

all_EU_ab_tidy %>% filter(is.na(Country)) %>% group_by(Code) %>% summarise(n=n()) # find codes not translated
# A new line to test if I can add things

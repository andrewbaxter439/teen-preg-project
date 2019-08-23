library(readxl)
library(tidyverse)
library(stringr)
library(broom)
library(magrittr)

birthRates <- read_csv("Downloaded data files/HFD_calc_births.csv")
allpops <- read_csv("Downloaded data files/HMD_allpops")

# Predictor #1 - GDPpercap -----------------------------------------------------------------------------------

GDP_all <- read_xlsx("Downloaded data files/GDPdata.xlsx", sheet = "Data", skip = 3)

ccodes <- read_tsv("country_codes.txt")

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

#** Check and correct country names ----------------------------------------------------------------------------

country_check <- tibble(country_names, country_names %in% GDP_all$'Country Name')

GDP_all[GDP_all$"Country Code" == "CZE",]$`Country Name` <- "Czechia"
GDP_all[GDP_all$"Country Code" == "USA",]$`Country Name` <- "United States of America"

GDP_all %>% 
  filter(`Country Name` %in% country_names) %>% 
  select(Country = `Country Name`, `1985`:`2017`) %>% 
  gather("Year", "GDPperCap", -1) %>% 
  # filter(is.na(GDPperCap)) -> NAcheck
  mutate(Year = as.numeric(Year)) -> GDP_cap


# Predictor #2 - m:f ratio of each age group -----------------------------------------------------------------

popAgeGrps <- tibble(Age=factor(c(13:15, 15:17, 15:19)),
                     agegrp=factor(c(1,1,1,2,2,2,3,3,3,3,3),
                                   labels=c("Under 16", "Under 18", "Under 20")))  # Three ranges of pop

popRatios <- 
  allpops %>%
  merge(popAgeGrps) %>%
  group_by(Code, Year, agegrp) %>% 
  summarise(MF_ratio = sum(Male)/sum(Female)) # create summary


# Urban population estimates ---------------------------------------------------------------------------------

df <- read_xls("Downloaded data files/UrbanPop.xls", sheet = "Data", skip = 3)


# Combine with birthRates ------------------------------------------------------------------------------------

synthData <- birthRates %>%
  left_join(GDP_cap, by = c("Country", "Year")) %>% 
  left_join(popRatios, by = c("Code", "Year", "agegrp")) %>%     # not useful?
  left_join(MobilePhones, by = c("Country", "Year")) %>% 
  left_join(UrbanPop, by = c("Country", "Year"))
  left_join(, by = c("Country", "Year"))
  left_join(, by = c("Country", "Year"))
  left_join(, by = c("Country", "Year"))
  
  
# write.csv(synthData, "Downloaded data files/SynthData in progress.csv")

# Group map to test change at 2008 ---------------------------------------------------------------------------

birthRates %>% 
  arrange(Country) %>%
  mutate(
    Year = as.numeric(Year),
    Time = Year - min(Year) + 1,
    Cat1 = ifelse(Year < 1999, 0, 1),
    Cat2 = ifelse(Year <= 2007, 0, 1),
    Trend1 = ifelse(Cat1 == 0, 0, Year - 1998),
    Trend2 = ifelse(Cat2 == 0, 0, Year - 2007)
  ) %>% 
  filter(agegrp == "Under 18", Cat1 == 1, Country != "Canada", Country != "Bulgaria") %>% 
  mutate(Time = Year - min(Year) + 1) %>%
  group_by(Country) %T>% 
  {print(group_map(., ~ tidy(lm(rate ~ Time + Trend2, data=.))) %>%
                arrange(desc(term), p.value, Country) %>% 
           assign("change08_mod", ., envir = .GlobalEnv))} %>%
  {ggplot(., aes(x = Time, y = rate, group = interaction(Country, Cat1, Cat2), col = Country)) +
  geom_smooth(method = "lm", se = FALSE)}
  
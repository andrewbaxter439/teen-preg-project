extractWB <- function(name) {
  
  require(readxl)
  require(dplyr)
  require(tidyr)
  
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

df <- read_xls(paste0("Downloaded data files/", name, ".xls"), sheet = "Data", skip = 3)

df[df$"Country Code" == "CZE",]$`Country Name` <- "Czechia"
df[df$"Country Code" == "USA",]$`Country Name` <- "United States of America"

dfTidy <- df %>% 
  filter(`Country Name` %in% country_names) %>% 
  select(Country = `Country Name`, `1985`:`2017`) %>% 
  gather("Year", "col", -1) %>% 
  mutate(Year = as.numeric(Year))

filter(dfTidy, is.na(col))

names(dfTidy)[names(dfTidy)=="col"] <- name


assign(paste0(name), dfTidy, envir = .GlobalEnv)

}


`Education Expenditure` %>% filter(Country=="United Kingdom") %>% pull(`Education Expenditure`) -> EE
`Education Expenditure` %>% filter(Country=="United Kingdom") %>% pull(Year) -> Yr


# approx(Yr, EE, xout = Yr[which(is.na(EE))], n = length(Yr), rule = 2) -> predEE
approx(Yr, EE, xout = 1985:2017, n = length(Yr), rule = 2) -> predEE

EEcomplete <- tibble(Yr, EE, predict = predEE$y)

# tibble(Yr, EE) %>% 
#   filter(!is.na(EE)) %>% 
#   bind_rows(tibble(Yr = predEE$x, EE = predEE$y)) %>% 
#   arrange(Yr)


# UK data across countries -----------------------------------------------------------------------------------

synthData[synthData$Code=="GBR_NIR",c("GDPperCap", "MobilePhones", "UrbanPop")] <- 
  synthData[synthData$Code=="GBR_NP",c("GDPperCap", "MobilePhones", "UrbanPop")]

synthData[synthData$Code=="GBR_SCO",c("GDPperCap", "MobilePhones", "UrbanPop")] <- 
  synthData[synthData$Code=="GBR_NP",c("GDPperCap", "MobilePhones", "UrbanPop")]

synthData[synthData$Code=="GBRTENW",c("GDPperCap", "MobilePhones", "UrbanPop")] <- 
  synthData[synthData$Code=="GBR_NP",c("GDPperCap", "MobilePhones", "UrbanPop")]

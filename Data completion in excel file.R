library(XLConnect)
library(tidyverse)

# Connect to Excel file --------------------------------------------------------------------------------------

databook <- loadWorkbook("T:/projects/Student-ABaxter/StudyDocuments_InDevelopment/Data sources.xlsx")

readWorksheet(databook, "data") %>% 
  colnames()

# Populating with country names ------------------------------------------------------------------------------

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

data.completion <- tibble(Country = country_names)


# Filling with other data ------------------------------------------------------------------------------------

data.completion <- teenbirths %>% 
  filter(Year>1984) %>%
  group_by(Country) %>%
  summarise(minYr = min(Year),
            maxYr = max(Year)) %>% 
  unite(2:3, col="Births", sep = " to ") %>% 
  right_join(data.completion, by="Country")

age.structure <- teenbirths %>% 
  group_by(Country) %>% 
  summarise(minAge = min(Age),
            maxYAge = max(Age)) %>% 
  unite(2:3, col="Births", sep = " to ")

# Saving Excel -----------------------------------------------------------------------------------------------


                          

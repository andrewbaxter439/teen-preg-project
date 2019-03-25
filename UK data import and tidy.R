library(tidyverse)
library(gganimate)
library(readxl)

# Import Scotland birth data ------------------------------------------------------------------------------

Scot.pop <- read_tsv(
  "Downloaded data files/SCOPop.txt"
) %>% 
  select(Year, Age, Total = Female)

Scot.conc <-
  read_xls(
    "Downloaded data files/ScotISD2018 report.xls",
    sheet = "Table1",
    range = "B33:P55",
    col_names = colnames(read_xls(
      "Downloaded data files/ScotISD2018 report.xls",
      sheet = "Table1",
      range = "B5:P6", col_names = TRUE))) %>%
  select(
    Year = 'Year of conception',
    '<16' = '<16 1',
    '<18' = '<18 2',
    '<20' = '<20 3'
  )

# Scotland births to estimate pregnancies pre-1994
# Data from NRS

Scot.births <-
  read_xlsx(
    "Downloaded data files/Scotland births.xlsx",
    range = "AQ4:BW16",
    col_names = TRUE
  )[c(3:10, 12), ] %>%
  select(Age = 'X__1', 1:32) %>%
   gather(Year, Births, -1) %>% 
  mutate_all(as.numeric) 


# Attempted function to calculate via weigted means ----------------------------------------------------------

est_conceptions <- function(year, age){
  tbl <- tibble(Year = c(year, year+1))
  tbl2 <-  tibble(Age = c(age, age+1))
  tbl3 <- merge(tbl, tbl2)
  tbl3 %>% 
    left_join(Scot.births) %>% 
    mutate(wt1 = c(0.25, 0.75, 0.25, 0.75),
           wt2 = 
    )
}

Scot.births %>%
  rowwise() %>% 
  mutate(Conceptions = (function(year, age)
  sum(0.25 * 0.25 * pull(Scot.births %>% filter(Age == age, Year == year) %>% select(Births)),
      0.25 * 0.75 * pull(Scot.births %>% filter(Age == age, Year == year+1) %>% select(Births)),
      0.75 * 0.25 * pull(Scot.births %>% filter(Age == age+1, Year == year) %>% select(Births)),
      0.75 * 0.75 * pull(Scot.births %>% filter(Age == age+1, Year == year+1) %>% select(Births)),
      na.rm = T)
)(Year, Age))
 
 
Scot.abort <- read_xlsx("Downloaded data files/mat_aas_table7.xlsx", skip=4) %>% 
  select(
    Year = '..1',
    'Under 20'
  ) %>% 
  na.omit() %>% 
  mutate(Year=as.numeric(gsub("(\\d\\d\\d\\d)(.*$)", "\\1", Year)))

sumScotBirths <- Scot.births %>%
  gather("Year", "Total",-1) %>%
  mutate(Year = as.numeric(Year)) %>%
  merge(birthAgeGrps) %>%
  group_by(Year, agegrp) %>%
  summarise(sumBirths = sum(Total)) # create summary

# merge with scotland births and population ---------------------------------------------------------------

Scot.birth.rates <- Scot.births %>%
  gather("Year", "Total",-1) %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(allpops %>%
              filter(Code == "GBR_SCO") %>%
              select(Age, Female, Year)) %>%
  merge(agecalcs) %>%
  group_by(Year, agegrp, agecat) %>%
  summarise(popsum = sum(Female), birthsum = sum(Total)) %>%
  ungroup() %>%
  filter(agegrp == agecat) %>%
  mutate(agecat = factor(agecat,
                         labels = c("Under 16", "Under 18", "Under 20"))) %>%
  mutate(rate = 1000 * birthsum / popsum)



library(pxR)
library(tidyverse)
library(readxl)


# alt pop data -----------------------------------------------------------------------------------------------

altpops <- read_xlsx("Downloaded data files/population_total.xlsx")
pop_perc <- read_xlsx("Downloaded data files/pop_female_perc.xlsx")

# Ireland births -----------------------------------------------------------------

Ire_births<-as.data.frame(read.px("Downloaded data files/IRE2007-2017.px"))

Ire2007<- tibble(Code="IRE", Year=2007, Age=c(15:20), sumBirths=c(69,158,397,723,1158,1359), Country="Ireland")
Ire2006<- tibble(Code="IRE", Year=2006, Age=c(15:20), sumBirths=c(48,151,365,676,1095,1325), Country="Ireland")
Ire2005<- tibble(Code="IRE", Year=2005, Age=c(15:20), sumBirths=c(42,182,388,772,1043,1252), Country="Ireland")
Ire2004<- tibble(Code="IRE", Year=2004, Age=c(15:20), sumBirths=c(53,202,399,779,1127,1339), Country="Ireland")
Ire2003<- tibble(Code="IRE", Year=2003, Age=c(15:20), sumBirths=c(58,187,489,852,1217,1394), Country="Ireland")
Ire2002<- tibble(Code="IRE", Year=2002, Age=c(15:20), sumBirths=c(63,225,504,932,1254,1517), Country="Ireland")
Ire2001<- tibble(Code="IRE", Year=2001, Age=c(15:20), sumBirths=c(67,214,520,975,1311,1530), Country="Ireland")

Ireprerates <- tibble(Year=c(1985:2000), Country="Ireland", agegrp=3, agecat="Under 20", rate=c(16.6,16.4,16.1,15.3,14.8,16.7,17.1,16.9,16.3,15.0,15.1,16.7,17.5,19.1,20.0,19.3)) %>%
  mutate(rate=rate/1000)

Ire_tidy <- Ire_births %>%
  filter(Statistic=="All Births (Number)" & Sex.of.Child == "Both sexes") %>%
  select(Year, Age.of.Mother, value) %>%
  mutate(Age.of.Mother = gsub(" years", "", Age.of.Mother),
         Age.of.Mother = ifelse(Age.of.Mother=="15 and under", "15", Age.of.Mother)) %>%
  filter(as.numeric(Age.of.Mother)>=15 & as.numeric(Age.of.Mother)<=20) %>%
  mutate(Age=as.numeric(Age.of.Mother), Total=as.numeric(value), Year=as.numeric(as.character(Year)), Country="Ireland", Code="IRE") %>%
  select(Code, Country, Year, Age, sumBirths = value)  %>%
  as_tibble() %>%
  bind_rows(Ire2007, Ire2006, Ire2005, Ire2004, Ire2003, Ire2002, Ire2001) %>%
  arrange(Year, Age)


# Australia births ------------------------------------------------------------------------
Aus2004 <- tibble(Code="AUS", Year=2004, Age=15:19, sumBirths=c(356,886,380,502,572), Country="Australia")
Aus1984to2003 <- read_tsv("Downloaded data files/Ausnuptcon.txt") %>%
  gather("Year", "Total", 2:10) %>%
  bind_rows(read_tsv("Downloaded data files/Ausexnuptcon.txt") %>%
              gather("Year", "Total", 2:10)) %>%
  group_by(Age, Year) %>%
  dplyr::summarise(sumBirths=sum(Total)) %>%
  mutate(Year=as.numeric(Year), Code = "AUS", Country = "Australia")

Aus_births <- read_excel("Downloaded data files/AUS teenage births.xlsx", sheet="Table 1.3", skip=5)
Aus_tidy <- Aus_births[2:7,] %>%
  select(Age = 1, 2:12) %>%
  mutate(Age = ifelse(Age=="Younger than 15 years", 14, as.numeric(gsub(" years","",Age)))) %>%
  gather("Year", "Total", 2:12) %>%
  mutate(Age=as.numeric(Age),
         sumBirths=as.numeric(Total),
         Year=as.numeric(as.character(Year)),
         Country="Australia",
         Code="AUS") %>%
  select(Code, Country, Year, Age, sumBirths) %>%
  bind_rows(Aus2004, Aus1984to2003) %>%
  arrange(Year, Age)

Ausprerates <- tibble(Year=c(1985:2003),
                      Country="Australia", agegrp=3, agecat="Under 20",
                      rate = c(22.8,21.8,20.6,20.3,20.6,22.1,22.1,22.0,20.9,20.7,20.4,20.1,19.8,18.9,18.5,17.7,17.7,17.4,16.3))



# New Zealand - abortions, stillbirths and livebirths combined ----------------------------------

NZ_import_rates <- read_csv("Downloaded data files/NZ_totalpregrates.csv", skip=2, col_names=c("Year", "Under 15", "Under 20"))[1:26,]

totpop_NZ <- altpops %>% filter(country == "New Zealand") %>% select(Country = country, `1992`:`2017`) %>% 
  gather("Year", "totpop", -1)

U_20_pop <- pop_perc %>% filter(country == "New Zealand") %>% select(Country = country, `1992`:`2017`) %>% 
  gather("Year", "pop_perc", -1) %>% 
  mutate(pop_perc = ifelse(is.na(.$pop_perc), mean(pop_perc, na.rm = TRUE), pop_perc)) %>% 
  left_join(totpop_NZ, by = c("Country", "Year")) %>% 
  mutate(sumPops = totpop * pop_perc / 100, agegrp = "Under 20") %>%
  select(Country, Year, agegrp, sumPops)
  
NZ_totalrates <- NZ_import_rates %>%
  select(Year, `Under 20`) %>% 
  gather("agegrp", "totalPregs", -1) %>% 
  left_join(U_20_pop, by = c("Year", "agegrp")) %>% 
  mutate(pRate = 1000*totalPregs/sumPops,
         Year = as.numeric(Year),
         Code = "NZL")
  
library(tidyverse)
library(readxl)


# Import Scotland data ---------------------------------------------------------------------------------------

Scotland.conc <- read_xls("Downloaded data files/ScotISD2018 report.xls", sheet = "Table1",
                          range = "B33:P55", col_names = FALSE) %>% 
  select(Year = '..1',
         '<16' = '..11',
         '<18' = '..13',
         '<20' = '..15')

#Scotland births to estimate pregnancies pre-1994

Scot.births <- read_xlsx("Downloaded data files/Scotland births.xlsx", range = "AQ4:BW16",
                          col_names = TRUE)[c(3:10,12),] %>% 
  select(Age = '..33', 1:32) %>% 
  mutate_all(as.numeric)

# Create age calculation groups

agegrps<-tibble(Age=factor(c(12:15, 12:17, 12:19)),
                agegrp=c(1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3)) # denominators for rate calc
agecats<-tibble(Age=factor(c(13:15, 15:17, 15:19)),
                agecat=c(1,1,1,2,2,2,3,3,3,3,3)) # numerators for rate calc
agecalcs<-merge(agegrps, agecats)

# merge with scotland births and population

Scot.birth.rates <- Scot.births %>% 
  gather("Year", "Total", -1) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  left_join(allpops %>% 
              filter(Code=="GBR_SCO") %>% 
              select(Age, Female, Year)) %>% 
  merge(agecalcs) %>% 
  group_by(Year, agegrp, agecat) %>%
  summarise(popsum=sum(Female), birthsum=sum(Total)) %>%
  ungroup() %>%
  filter(agegrp==agecat) %>%
  mutate(agecat=factor(agecat,
                       labels = c("Under 16", "Under 18", "Under 20"))) %>%
  mutate(rate=1000*birthsum/popsum)

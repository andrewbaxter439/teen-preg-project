library(gganimate)

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
    col_names = FALSE
  ) %>%
  select(
    Year = '..1',
    '<16' = '..11',
    '<18' = '..13',
    '<20' = '..15'
  )

# Scotland births to estimate pregnancies pre-1994
# Data from NRS

Scot.births <-
  read_xlsx(
    "Downloaded data files/Scotland births.xlsx",
    range = "AQ4:BW16",
    col_names = TRUE
  )[c(3:10, 12), ] %>%
  select(Age = '..33', 1:32) %>%
   gather(Year, Births, -1) %>% 
   # mutate(Year=as.numeric(Year)
  mutate_all(as.numeric) %>% 
  mutate(YrP = Year-1,
          AgP = Age-1)

Scot.births.est <-  Scot.births %>% 
   mutate(Conceptions = (Births / 16 + 
            Scot.births[(Scot.births$Year==YrP)&(Scot.births$Age==Age),'Births']*3/16))
 
 
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



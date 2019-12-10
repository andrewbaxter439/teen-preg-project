library(tidyverse)
library(gganimate)
library(readxl)
library(broom)

estConceptions <- function(year, age){
  # Function to create estimated conceptions from weighted means for year = 'year' and age = 'age'
  # returns: numeric value of estimated conceptions
  tbl <- tibble(Year = c(year, year+1))
  tbl2 <-  tibble(Age = c(age, age+1))
  tbl3 <- merge(tbl, tbl2)
  tbl4 <- tbl3 %>% left_join(Scot.births) 
  tbl4 %>% 
    mutate(wt1 = c(0.27, 0.73, 0.27, 0.73),  # Weighting by probability of birth in same year as conc. (14/52)
           wt2 = c(0.27, 0.27, 0.73, 0.73),  # Weighting by probability of birth at same age as conc.
           wt3 = c(sum(tbl4[Age==age, "Births"])/sum(tbl4[, "Births"],   na.rm = TRUE),  # Weighting by probability
                   sum(tbl4[Age==age, "Births"])/sum(tbl4[, "Births"],   na.rm = TRUE),  # increasing with age -
                   sum(tbl4[Age==age+1, "Births"])/sum(tbl4[, "Births"], na.rm = TRUE),  # proportion at 'age'
                   sum(tbl4[Age==age+1, "Births"])/sum(tbl4[, "Births"], na.rm = TRUE)),
           wtTot = wt1*wt2*wt3
    ) %>% 
    summarise(Conceptions = weighted.mean(Births, w=wtTot)) %>% 
    pull()
}
# Grouping births and populations setup ---------------------------------------------------------------------

birthAgeGrps <- tibble(Age=factor(c(12:15, 12:17, 12:19)),
                       agegrp=factor(c(1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3),
                                     labels=c("Under 16", "Under 18", "Under 20")))  # Three ranges of births


popAgeGrps <- tibble(Age=factor(c(13:15, 15:17, 15:19)),
                     agegrp=factor(c(1,1,1,2,2,2,3,3,3,3,3),
                                   labels=c("Under 16", "Under 18", "Under 20")))  # Three ranges of pop

# Combine pop and births and calculate rates ------------------------------

sumBirths <- teenBirths %>% 
  merge(birthAgeGrps) %>% 
  group_by(Code, Country, Year, agegrp) %>% 
  summarise(sumBirths = sum(Total)) # create summary

sumPops <- allpops %>%
  merge(popAgeGrps) %>%
  group_by(Code, Year, agegrp) %>% 
  summarise(sumPops = sum(Female)) # create summary

birthRates <-left_join(sumBirths, sumPops,
                       by=c("Year","agegrp","Code")) %>%
  filter(Year>1984, Country %in% country_names) %>%
  mutate(rate=1000*sumBirths/sumPops)

# Import Scotland birth data ------------------------------------------------------------------------------

Scot.pop <- read_tsv(
  "Downloaded data files/SCOPop.txt"
) %>% 
  select(Year, Age, Total = Female)

sumScot.pop <- Scot.pop %>% 
  inner_join(popAgeGrps, by="Age") %>% 
  group_by(Year, agegrp) %>% 
  summarise(Population = sum(Total))

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

# Scotland births to estimate pregnancies pre-1994 ------------------
# Data from NRS

Scot.births <-
  read_xlsx(
    "Downloaded data files/Scotland births.xlsx",
    range = "AQ4:BW16",
    col_names = TRUE
  )[c(3:10, 12), ] %>%
  select(Age = '...33', 1:32) %>%
   gather(Year, Births, -1) %>% 
  mutate_all(as.numeric) 

# Estimate births for age groups ------------------

Scot.births %>% 
  rowwise %>% 
  mutate(Conceptions = estConceptions(Year, Age)) %>% 
  merge(birthAgeGrps) %>% 
  group_by(Year, agegrp) %>% 
  summarise(Deliveries = sum(Conceptions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  assign("Scot.births.adj", ., envir = .GlobalEnv)

# Import ISD to check accuracy ----------------------------------

Scot.births.ISD <- read_xls("Downloaded data files/mat_tp_table4.xls", range = "B5:K29")[3:24, ] %>% 
  select(Year = 'Year of conception',
         'Under 16_del' = 3,
         'Under 16_abo' = 4,
         'Under 18_del' = 6,
         'Under 18_abo' = 7,
         'Under 20_del' = 9,
         'Under 20_abo' = 10)

#** Applying correction through mean ratio diffs ------------------

Scot.births.ISD %>% 
  gather("cat", "value", 2:7) %>% 
  separate(cat, c("agegrp", "cat"), sep = "_") %>% 
  spread(cat, value) %>% 
  mutate(agegrp = factor(agegrp),
         Deliveries = as.numeric(del)) %>%
  left_join(Scot.births.adj, by=c("Year", "agegrp"), suffix = c(".rec", ".est")) %>% 
  group_by(agegrp) %>% 
  mutate(est_diff = Deliveries.est-Deliveries.rec,
         est_ratio = Deliveries.rec/Deliveries.est) %>%
  group_by(agegrp) %>% 
  mutate(min_diff = min(est_diff), max_diff=max(est_diff),
         min_ratio = min(est_ratio), max_ratio=max(est_ratio),
         mean_ratio = mean(est_ratio),
         Deliveries.corr = Deliveries.est*mean_ratio) %>% 
  assign("Scot.births.corr", ., envir = .GlobalEnv) %T>% 
  {print(ggplot(data = .,aes(Year)) +
  geom_line(aes(y = Deliveries.rec, col = "Recorded deliveries")) +
  geom_line(aes(y = Deliveries.est, col = "Estimated deliveries")) +
  geom_line(aes(y = Deliveries.corr, col = "Corrected deliveries")) +
  facet_wrap(~agegrp))} %>% 
  head()

#** Applying correction through lm predicting rec from est ---------------------

Scot.births.ISD %>% 
  gather("cat", "value", 2:7) %>% 
  separate(cat, c("agegrp", "cat"), sep = "_") %>% 
  spread(cat, value) %>% 
  mutate(agegrp = factor(agegrp),
         Deliveries = as.numeric(del)) %>%
  left_join(Scot.births.adj, by=c("Year", "agegrp"), suffix = c(".rec", ".est")) %>% 
  group_by(agegrp) %>% 
  group_modify(., ~ tibble(factor = summary(lm(Deliveries.rec ~  0 + Deliveries.est, data = .x))[["coefficients"]][1],
           Rsqr = summary(lm(Deliveries.rec ~ 0 + Deliveries.est, data = .x))[["r.squared"]]),
           keep = TRUE
    ) %>%  # old-style of group_map - no longer works
  right_join(Scot.births.corr, by = "agegrp") %>% 
  mutate(Deliveries.corr2 = Deliveries.est*factor) %>% 
  assign("Scot.births.corr", ., envir = .GlobalEnv) %T>% 
  {print(ggplot(data = .,aes(Year)) +
  geom_line(aes(y = Deliveries.rec, col = "Recorded deliveries")) +
  geom_line(aes(y = Deliveries.est, col = "Estimated deliveries")) +
  geom_line(aes(y = Deliveries.corr2, col = "Corrected deliveries")) +
  facet_wrap(~agegrp))} %>% 
  head()

#** Check R^2 of each calculation type ---------------

Scot.births.corr %>% 
  group_by(agegrp) %>% 
  summarise(Rsqr1 = (sum((Deliveries.rec-mean(Deliveries.rec))^2)-sum((Deliveries.rec - Deliveries.corr)^2))/sum((Deliveries.rec-mean(Deliveries.rec))^2),
            Rsqr1b = (function(x, y) cor(x, y)^2)(Deliveries.rec, Deliveries.corr),
            Rsqr2 = (sum((Deliveries.rec-mean(Deliveries.rec))^2)-sum((Deliveries.rec - Deliveries.corr2)^2))/sum((Deliveries.rec-mean(Deliveries.rec))^2),
            Rsqr2b = (function(x, y) cor(x, y)^2)(Deliveries.rec, Deliveries.corr2))

#** Sum births by age group ----------------------------- 
## Using multiplication factor derived from lm

sumScotBirths <- Scot.births.corr %>% 
  select(agegrp, factor) %>% 
  unique() %>% 
  inner_join(Scot.births.adj,., by = "agegrp") %>% 
  filter(Deliveries != 0) %>% 
  mutate(Deliveries.corr = Deliveries * factor) #%>% 
  # ggplot(aes(Year)) +
  # geom_line(aes(y = Deliveries.corr, col = "Corrected deliveries")) +
  # geom_line(data = Scot.births.corr, aes(y = Deliveries.rec, col = "Recorded deliveries")) +
  # facet_wrap(~agegrp)
 
#** Read in abortion data for under 20s ----------------------------------

Scot.abort <- read_xlsx("Downloaded data files/mat_aas_table7.xlsx", skip=4) %>% 
  mutate(agegrp = "Under 20") %>% 
  select(
    Year = '...1',
    agegrp,
    Abortions = 'Under 20'
  ) %>% 
  na.omit() %>% 
  mutate(Year=as.numeric(gsub("(\\d\\d\\d\\d)(.*$)", "\\1", Year)))

# Correction factor for abortion rate (to account for abortions after turning 20)

Scot.data <- read_xlsx("Downloaded data files/AllUK1994-2016.xlsx", sheet = "scot_agegrp")

ab20.correction <- Scot.abort %>% 
  left_join(Scot.data, by=c("Year", "agegrp")) %>% 
  select(Year, Abortions.x, Abortions.y) %>% 
  filter(!is.na(Abortions.y)) %>% 
  summarise(factor = summary(lm(Abortions.y ~ 0 + Abortions.x, data=.))[["coefficients"]][1]) %>% 
  pull()

Scot.abort <- Scot.abort %>% 
  mutate(Abortions.corr = Abortions * ab20.correction)

# merge with population to create rates for under-20s -------------------------------------------------------

scotConceptionsUnde20 <- sumScotBirths %>% 
  filter(agegrp == "Under 20") %>% 
  select(Year, agegrp, Births = Deliveries.corr) %>% 
  left_join(Scot.abort) %>% 
  mutate(Total = Births + Abortions.corr) %>% 
  left_join(sumScot.pop) %>% 
  mutate(Value = 1000*Total/Population)

write_csv(scotConceptionsUnde20, "EstScot_1985_2015.csv")



# England and Wales rates ------------------------------------------------------------------------------------

years <- tibble(Year = 1985:2016)
countries <- tibble(Country = c("England", "Wales", "England and Wales"))
agegrps <- tibble(agegrp = c("Under 16", "Under 18", "Under 20"))

emptySet <- merge(years, countries) %>% merge(agegrps)
initialSet <- read_xlsx("Downloaded data files/EngWalConceptions1998-2015.xlsx", sheet = "by_agegrp") %>% 
  mutate(Year = as.numeric(Year))

initialSet %>% 
  select(1:4) %>% 
  spread(Country, Conceptions) %>% 
  arrange(desc(Year))

emptySet %>% anti_join(initialSet, by=c("Year", "agegrp", "Country"))

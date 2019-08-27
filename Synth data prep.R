library(readxl)
library(tidyverse)
library(stringr)
library(broom)
library(magrittr)
library(Synth)
library(Rcpp)
library(purrr)
library(svglite)
library(SPHSUgraphs)
`-.gg` <- function(e1, e2) e2(e1)

# data import ------------------------------------------------------------------------------------------------


birthRates <- read_csv("Downloaded data files/HFD_calc_births.csv")

allpops <- read_csv("Downloaded data files/HMD_allpops")

ccodes %>% filter(Country %in% country_names) %>% 
  left_join(allpops, by = "Code") %>% filter(Country == "New Zealand")

allUKrates_U20 <-
  read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 20")

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

UrbanPop <- read_xls("Downloaded data files/UrbanPop.xls", sheet = "Data", skip = 3)


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
synthData <- read.csv("Downloaded data files/SynthData in progress.csv")  

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
  {print(group_modify(., ~ tidy(lm(rate ~ Time + Trend2, data=.))) %>%
                arrange(desc(term), p.value, Country) %>%
           assign("change08_mod", ., envir = .GlobalEnv))} %>%
  {ggplot(., aes(x = Time, y = rate, group = interaction(Country, Cat1, Cat2), col = Country)) +
  geom_smooth(method = "lm", se = FALSE)}


# Synth prep -------------------------------------------------------------------------------------------------

synthData[synthData$Country == "Spain" &
            synthData$agegrp == "Under 18" & 
            synthData$Year == 1985, "MobilePhones"] <- 0


excl_countries <- synthData_U18 %>% 
  filter(is.na(GDPperCap)) %>% 
  select(Country) %>% 
  unique() %>% 
  pull()

synthData_U18 %>% select(Country, Year, rate) %>% spread(Year, rate)  # test missing 'rate' values

synthData_U18 <- synthData %>%
  filter(agegrp == "Under 18",
         !Country %in% c("United Kingdom", "Croatia", "Bulgaria", "Canada")) %>%  # countries and years with missing data
  mutate(Country = as.character(Country),
         Code = as.numeric(Code))

synthData_U18[which(synthData_U18$Country=="Poland" & synthData_U18$Year==2001),"rate"] <- 
mean(synthData_U18[which(synthData_U18$Country=="Poland" & synthData_U18$Year==2000),"rate"],
synthData_U18[which(synthData_U18$Country=="Poland" & synthData_U18$Year==2002),"rate"])
  

controls <- synthData_U18 %>% 
  filter(Country != "England and Wales") %>%
  select(Code) %>% 
  # transmute(Country = as.character(Country)) %>% 
  unique() %>% 
  pull()

u_18_ccodes <- synthData_U18 %>% 
  select(Code, Country) %>% 
  unique()


# ** Adding abortions to under-20 ----------------------------------------------------------------------------

# ccodes_new <- ccodes %>% 
#   bind_rows(tibble(Code = c("GBR", "FRA", "DEU"),
#                    Country = c("United Kingdom", "France", "Germany")))

abortions <- read_csv("Downloaded data files/EHIG_abortions.csv", skip = 25)
abortions_tidy <- abortions %>% 
  filter(!is.na(VALUE),
         is.na(COUNTRY_GRP)) %>% 
  select(Code = COUNTRY,Year = YEAR, Abortions = VALUE) %>% 
  mutate(agegrp = "Under 20",
         Code = ifelse(Code == "FRA", "FRATNP",
                       ifelse(Code == "DEU", "DEUTNP",
                              ifelse(Code == "GBR", "GBR_NP", Code)))) %>% 
  left_join(ccodes_new, by = "Code") %>% 
  select(Code, Country, Year:agegrp) %>% 
  semi_join(synthData, by = c("Code", "Country", "Year", "agegrp"))

ab_missing <- abortions_tidy %>% 
  spread(Year, Abortions)
  
ab_missing <- ab_missing %>%
  mutate(nMissing = rowSums(is.na(ab_missing))) %>% 
  select(Country, nMissing)

synth_data_plus_ab <- right_join(abortions_tidy, synthData, by = c("Code", "Country", "Year", "agegrp"))

# ** Function to interpolate missing ab values ---------------------------------------------------------------

# data <- synth_data_plus_ab
# country <- "Bulgaria"

interpolateAb <- function(country, data = synth_data_plus_ab){
cdata_u20 <- data %>% 
  filter(Country == country, agegrp == "Under 20")

meanProp <- cdata_u20 %>% mutate(prop = Abortions/sumBirths) %>% 
  filter(prop < mean(prop, na.rm = TRUE) + 2*sd(prop, na.rm = TRUE)) %>% 
  summarise(mean = mean(prop)) %>% pull()

new_dat <- cdata_u20 %>% 
  filter(is.na(Abortions)) %>%
  mutate(Abortions = sumBirths * meanProp) %>% 
  full_join(cdata_u20 %>% filter(!is.na(Abortions)), by = c("Code", "Country", "Year", "Abortions", "agegrp", "sumBirths", "sumPops", "rate", "GDPperCap", "MF_ratio", "MobilePhones", "UrbanPop")) %>% 
  mutate(totalPregs = Abortions + sumBirths)

data %>% 
  filter(Country == country, agegrp != "Under 20") %>% 
  bind_rows(new_dat,.) %>% 
  return()

}


# ** Complete dataframe --------------------------------------------------------------------------------------

synth_data_interp_ab <- synth_data_plus_ab %>% 
  select(Country) %>% 
  unique() %>% 
  pull() %>% 
  map_dfr(., ~interpolateAb(.)) %>% 
  mutate(pRate = 1000*totalPregs/sumPops)

Eng_rates <- allUKrates_U20 %>% 
  filter(Country == "England and Wales") %>% 
  gather("Year", "pRate", -1) %>% 
  mutate(Year = as.numeric(Year))

synth_data_interp_ab %<>%
  filter(Country == "England and Wales", agegrp == "Under 20") %>% 
  select(-pRate) %>% 
  left_join(Eng_rates, by = c("Country", "Year")) %>% 
  bind_rows(synth_data_interp_ab %>% filter(Country != "England and Wales" | agegrp != "Under 20"))

Sco_rates <- allUKrates_U20 %>% 
  filter(Country == "Scotland") %>% 
  gather("Year", "pRate", -1) %>% 
  mutate(Year = as.numeric(Year))

synth_data_interp_ab %<>%
  filter(Country == "Scotland", agegrp == "Under 20") %>% 
  select(-pRate) %>% 
  left_join(Sco_rates, by = c("Country", "Year")) %>% 
  bind_rows(synth_data_interp_ab %>% filter(Country != "Scotland" | agegrp != "Under 20"))


US_rates <- allUKrates_U20 %>% 
  filter(Country == "U.S.A.") %>% 
  gather("Year", "pRate", -1) %>% 
  mutate(Year = as.numeric(Year),
         Country = "United States of America")

synth_data_interp_ab %<>%
  filter(Country == "United States of America", agegrp == "Under 20") %>% 
  select(-pRate) %>% 
  left_join(US_rates, by = c("Country", "Year")) %>% 
  bind_rows(synth_data_interp_ab %>% filter(Country != "United States of America" | agegrp != "Under 20"))

synth_data_interp_ab %<>%
  bind_rows(NZ_totalrates, .)

# ** Model 1: rate only as predictor -------------------------------------------------------------------------

dp_rate <- dataprep(
  foo = synthData_U18 %>% filter(Year<2014),
  predictors = c("rate"),
  predictors.op = "mean",
  time.predictors.prior = 1985:1998,
  dependent = "rate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = u_18_ccodes[u_18_ccodes$Country=="England and Wales", "Code"],
  controls.identifier = u_18_ccodes[u_18_ccodes$Country!="England and Wales", "Code"],
  time.optimize.ssr = 1996:1998,
  time.plot = 1985:2013
)

md_rate <- predvalues_synth(dp_rate)
# so_rate <- synth(dp_rate)
# 
# st_rate <- synth.tab(dataprep.res = dp_rate, synth.res = so_rate)  

st_rate$tab.v
st_rate$tab.w
st_rate$tab.pred

path.plot(dataprep.res = dp_rate, synth.res = so_rate)


gg_synth(md = md_rate, post = TRUE)
pl_u18_rate <- generatePlacebos(synthData_U18 %>% filter(Year < 2014), predictors = "rate", time.optimize.ssr = 1996:1998)
mspe_limit_rate <- pre_MSPE(md_rate)

md_rate %>% spread(Group, Rate) %>% 
  mutate(Gap = Treated - Synthetic) %>% 
  ggplot(aes(Year, Gap)) +
  geom_line(data = pl_u18_rate %>% filter(pre_mspe < 10*mspe_limit_rate), aes(group = Country), col = "grey") +
  geom_line(col = "blue", size = 2) +
  theme_minimal() +
  geom_vline(xintercept = 1998.5, linetype = "dotted")


# **** Model 1b: Rate special predictors -------------------------------------------------

dp_U18_rateSp <- dataprep(
  foo = synthData_U18 %>% filter(Year< 2014),
  special.predictors = list(
    list("rate", 1985:1989, "mean"),
    list("rate", 1990:1995, "mean"),
    list("rate", 1996:1998, "mean")
    ),
  predictors.op = "mean",
  time.predictors.prior = 1985:1998,
  dependent = "rate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls,
  time.optimize.ssr = 1990:1998,
  time.plot = 1985:2013
)

md_U18_rateSp <- predvalues_synth(dp_U18_rateSp)

# so_rateSp <- synth(dp_rateSp)
# st_rateSp <- synth.tab(so_rateSp, dp_rateSp)  


gr_U18_rateSP <- gg_synth(md = md_U18_rateSp, post = TRUE) 


gr_U18_rateSP + labs(title = sPredText(dp_U18_rateSp))

ggsave("graphs/Under-18 rates - inc Scot+NI - no pred.png")


printCoefficients(md_rateSp)

pl_u18_rateSp <- generatePlacebos(synthData_U18 %>% filter(Year<2014),   special.predictors = list(
  list("rate", 1985:1989, "mean"),
  list("rate", 1990:1995, "mean"),
  list("rate", 1996:1998, "mean")
),
)

mspe_limit_rateSp <- pre_MSPE(md_rateSp)

md_rateSp %>% 
  spread(Group, Rate) %>% 
  mutate(Gap = Treated - Synthetic) %>% 
  ggplot(aes(Year, Gap)) +
  geom_line(data = pl_u18_rateSp %>% filter(pre_mspe < 20*mspe_limit_rateSp), aes(group = Country), col = "grey") +
  geom_line(col = "blue", size = 2) +
  theme_minimal() +
  geom_vline(xintercept = 1998.5, linetype = "dotted")


st_rateSp$tab.v
st_rateSp$tab.w
st_rateSp$tab.pred


# **** Model 1b.2: Rate special predictors optim -------------------------------------------------

dp_U18_rateSp.2 <- dataprep(
  foo = synthData_U18 %>% filter(Year< 2014),
  special.predictors = list(
    list("rate", 1985:1987, "mean"),
    list("rate", 1988:1990, "mean"),
    list("rate", 1991:1993, "mean"),
    list("rate", 1994:1998, "mean")
    ),
  predictors.op = "mean",
  time.predictors.prior = 1985:1998,
  dependent = "rate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls,
  time.optimize.ssr = 1990:1998,
  time.plot = 1985:2013
)

md_U18_rateSp.2 <- predvalues_synth(dp_U18_rateSp.2)



mspe_limit_U18_rateSp.2 <- so_U18_rateSp.2$loss.v[1]

gr_U18_rateSP.2 <- gg_synth(md = md_U18_rateSp.2, post = TRUE, mspe = mspe_limit_U18_rateSp.2) 


gr_U18_rateSP.2 + labs(title = sPredText(dp_U18_rateSp.2))

ggsave("graphs/Under-18 rates - inc Scot+NI - no pred.png")


printCoefficients(md_rateSp.2)

pl_u18_rateSp.2 <- generatePlacebos(synthData_U18 %>% filter(Year<2014),   special.predictors = list(
  list("rate", 1985:1987, "mean"),
  list("rate", 1988:1990, "mean"),
  list("rate", 1991:1993, "mean"),
  list("rate", 1994:1998, "mean")
),
)


md_U18_rateSp.2 %>% 
  spread(Group, Rate) %>% 
  mutate(Gap = Treated - Synthetic) %>% 
  ggplot(aes(Year, Gap)) +
  geom_line(data = pl_u18_rateSp.2 %>% filter(pre_mspe < 5*mspe_limit_U18_rateSp.2), aes(group = Country), col = "grey") +
  geom_line(col = sphsu_cols("Thistle", names = FALSE), size = 2) +
  theme_minimal() +
  geom_vline(xintercept = 1998.5, linetype = "dotted") +
  labs(title = sPredText(dp_U18_rateSp.2), subtitle = paste0("MSPE pre-intervention: ", round(mspe_limit_U18_rateSp.2, 3), " (controls <5*MSPE)"))

ggsave("graphs/Under-18 rates - inc Scot+NI - placebo test.png")

st_U18_rateSp.2$tab.v
st_U18_rateSp.2$tab.w
st_U18_rateSp.2$tab.pred


# **** Model 1c: rates relative to 1985 --------------------------------------------------------------------

synthData_U18_r <- synthData_U18 %>%
  group_by(Code) %>% 
  slice(6) %>% 
  select(Code, base = rate) %>% 
  right_join(synthData_U18, by = "Code") %>% 
  mutate(rRate = rate/base) %>% 
  select(-base) %>% 
    as.data.frame()

dp_relative <- dataprep(
  foo = synthData_U18_r,
  # predictors = c("rRate"),
  special.predictors = list(
    # list("rRate", 1990:1995, "mean"),
    list("rRate", 1996:1998, "mean")
    ),
  predictors.op = "mean",
  time.predictors.prior = 1990:1998,
  dependent = "rRate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls,
  time.optimize.ssr = 1996:1998,
  time.plot = 1990:2013
)

so_relative <- synth(dp_relative)

st_relative <- synth.tab(so_relative, dp_relative)  

md_relative <- predvalues_synth(dp_relative)

(gr_relative <- md_relative %>% 
  ggplot(aes(Year, Rate, col = Group, linetype = Group)) +
  geom_line(size = 1.5) +
  theme_sphsu_light() +
  ylab("Under-18 birth rate (per 1,000 women)") +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank()) +
  scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
  scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
  geom_vline(xintercept = 1998.5, linetype = "dotted"))

export::graph2ppt(gr_relative, "Under-18 birth rates relative to 1985.pptx", height = 5.13, width = 5.36)

st_relative$tab.v
st_relative$tab.w
st_relative$tab.pred


# **** Model 1d: Rate special predictors - No Scotland -------------------------------------------------

dp_U18_noScot <- dataprep(
  foo = synthData_U18 %>% filter(Year< 2014),
  special.predictors = list(
    list("rate", 1985:1989, "mean"),
    list("rate", 1990:1993, "mean"),
    list("rate", 1994:1995, "mean"),
    list("rate", 1996:1998, "mean")
    ),
  predictors.op = "mean",
  time.predictors.prior = 1985:1998,
  dependent = "rate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = u_18_ccodes[u_18_ccodes$Country=="England and Wales", "Code"],
  controls.identifier = u_18_ccodes[u_18_ccodes$Country!="England and Wales"&u_18_ccodes$Country!="Scotland", "Code"],
  time.optimize.ssr = 1985:1998,
  time.plot = 1985:2013
)

md_U18_noScot <- predvalues_synth(dp_U18_noScot)

gg_synth(md = md_U18_noScot) + labs(title = sPredText(dp_U18_noScot))

st_U18_noScot$tab.w
st_U18_noScot$tab.v
st_U18_noScot$tab.pred


gr_U18_noScot <- gg_synth(md = md_U18_noScot, post = TRUE)

gr_U18_noScot + labs(title = sPredText(dp_U18_noScot))


ggsave("graphs/Under-18 rates - excl Scot - no pred.png")




# **** Model 1e: Rate special predictors - no Scotland/NI ----------------------------------------------

dp_U18_noScotNI <- dataprep(
  foo = synthData_U18 %>% filter(Year< 2014),
  special.predictors = sp_U18_noScotNI,
  predictors.op = "mean",
  time.predictors.prior = 1985:1998,
  dependent = "rate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = u_18_ccodes[u_18_ccodes$Country=="England and Wales", "Code"],
  controls.identifier = u_18_ccodes[u_18_ccodes$Country!="England and Wales"&u_18_ccodes$Country!="Scotland"&u_18_ccodes$Country!="Northern Ireland", "Code"],
  time.optimize.ssr = 1990:1998,
  time.plot = 1985:2013
)

md_U18_noScotNI <- predvalues_synth(dp_U18_noScotNI)



gr_U18_noScotNI <- gg_synth(md = md_U18_noScotNI, post = TRUE, mspe = mspe_limit_U18_noScotNI)

gr_U18_noScotNI +
  labs(title = sPredText(dp_U18_noScotNI))
  

st_U18_noScotNI$tab.w
st_U18_noScotNI$tab.v
st_U18_noScotNI$tab.pred

ggsave("graphs/Under-18 rates - excl Scot-NI - no pred.png")



printCoefficients(md_noScotNI)

pl_u18_noScotNI <- generatePlacebos(synthData_U18 %>% filter(Year<2014),   special.predictors = sp_U18_noScotNI
)

mspe_limit_U18_noScotNI <- so_U18_noScotNI$loss.v[1,1]

md_U18_noScotNI %>% 
  spread(Group, Rate) %>% 
  mutate(Gap = Treated - Synthetic) %>% 
  ggplot(aes(Year, Gap)) +
  geom_line(data = pl_u18_noScotNI %>% filter(pre_mspe < (5*mspe_limit_U18_noScotNI)), aes(group = Country), col = "grey") +
  geom_line(col = sphsu_cols("Thistle", names = FALSE), size = 2) +
  theme_minimal() +
  geom_vline(xintercept = 1998.5, linetype = "dotted") +
  labs(title = sPredText(dp_U18_rateSp.2), subtitle = paste0("MSPE pre-intervention: ", round(mspe_limit_U18_rateSp.2, 3), " (controls <5*MSPE)"))

ggsave("graphs/Under-18 rates - No scot+NI - placebo test.png")


# ** Model 2: add in GDPperCap -------------------------------------------------------------------------------

synthData_U18_b <- synthData %>%
  filter(Country=="United Kingdom", agegrp == "Under 18", Year < 2014) %>% 
  mutate(Country = "England and Wales") %>% 
  select(Country, Year, GDPperCap, MobilePhones, UrbanPop) %>% 
  left_join(synthData_U18 %>% filter(Country == "England and Wales") %>% select(-GDPperCap, -MobilePhones, -UrbanPop), by = c("Country", "Year")) %>% 
  bind_rows(synthData_U18 %>% filter(Country != "England and Wales"))

dp_u18_gdp <- dataprep(
  foo = synthData_U18_b,
  predictors = c("GDPperCap"),
  special.predictors = list(
    list("rate", 1985:1995, "mean"),
    list("rate", 1996:1998, "mean")
    ),
  predictors.op = "mean",
  time.predictors.prior = 1985:1998,
  dependent = "rate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls,
  time.optimize.ssr = 1996:1998,
  time.plot = 1990:2013
)

so_u18_gdp <- synth(dp_u18_gdp)

st_u18_gdp <- synth.tab(so_u18_gdp, dp_u18_gdp)  

md_u18_gdp <- predvalues_synth(dp_u18_gdp)

(gr_u18_gdp <- md_u18_gdp %>% 
  ggplot(aes(Year, Rate, col = Group, linetype = Group)) +
  geom_line(size = 1.5) +
  theme_sphsu_light() +
  ylab("Under-18 birth rate (per 1,000 women)") +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank()) +
  scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
  scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
  geom_vline(xintercept = 1998.5, linetype = "dotted"))

export::graph2ppt(gr_u18_gdp, "Under-18 birth rates with GDPpercap as pred.pptx", height = 6, width = 9.5)

st_u18_gdp$tab.v
st_u18_gdp$tab.w
st_u18_gdp$tab.pred



# ** Model 3: all predictors ---------------------------------------------------------------------------------

dp_all <- dataprep(
  foo = synthData_U18_b,
  predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"),
  special.predictors = list(
    list("rate", 1985:1995, "mean"),
    list("rate", 1996:1998, "mean")
  ),
  predictors.op = "mean",
  time.predictors.prior = 1985:1998,
  dependent = "rate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls,
  time.optimize.ssr = 1996:1998,
  time.plot = 1990:2013
)

so_all <- synth(dp_all)

st_all <- synth.tab(so_all, dp_all)
st_all$tab.v
st_all$tab.w

md_all <- predvalues_synth(dp_all)

gg_synth(dp_all)
printCoefficients(md_all)

# Synth for total Under 20 pregnancies -----------------------------------------------------------------------

synthData_U20 <- synth_data_interp_ab %>% 
  filter(agegrp == "Under 20") %>% 
  select(Code, Country, Year, GDPperCap, pRate) %>% 
  filter(!Country %in% c("United Kingdom", "Austria", "Croatia", "Canada", "Northern Ireland", "Bulgaria"),
         Year > 1984,
         Year < 2014) %>% 
  mutate(Code = as.numeric(factor(Code, ordered = TRUE))) %>% 
  arrange(Code)


# filling in missing data
synthData_U20[synthData_U20$Country == "Poland" & synthData_U20$Year == 2001, 'pRate'] <- 
  mean(synthData_U20[[which(synthData_U20$Country == "Poland" & synthData_U20$Year == 2000), 'pRate']],
       synthData_U20[[which(synthData_U20$Country == "Poland" & synthData_U20$Year == 2002), 'pRate']])

synthData_U20[synthData_U20$Country == "Scotland" & synthData_U20$Year %in% 1985:1993, 'pRate'] <- read_csv("Downloaded data files/EstScot_1985_2015.csv") %>%
  filter(Year > 1984, Year < 1994) %>% 
  select(Value) %>% 
  pull()

synthData_U20 <- bind_rows(synthData_U20,
  tibble(Code = 17, 
                   Country = "New Zealand", 
                   Year = 1990:1991, 
                   pRate = synthData_U20[[which(synthData_U20$Country=="New Zealand" &
                                                  synthData_U20$Year == 1992), 'pRate']])
)

synthData_U20 <- synthData_U20 %>%
  filter(Country == "New Zealand") %>% 
  select(-GDPperCap) %>% 
  left_join(GDP_cap, by = c("Country", "Year")) %>% 
  bind_rows(synthData_U20 %>% filter(Country != "New Zealand"))

check_sd <- synthData_U20 %>% select(Country, Year, pRate) %>% spread(Year, pRate)


controls_U20 <- synthData_U20 %>% 
  filter(Country != "England and Wales") %>% 
  select(Code) %>% 
  unique() %>% 
  pull()

u_20_codes <- synthData_U20 %>% 
  select(Country, Code) %>% 
  unique()

synthData_U20 %>% ggplot(aes(Year, pRate, col = Country)) + geom_line(size = 2) + scale_colour_sphsu() - ggplotly

# ** U20 basic synth -----------------------------------------------------------------------------------

dp_u20_scot <- dataprep(
  foo = data.frame(synthData_U20 %>% filter(Year > 1989)),
  predictors = c("pRate"),
  # special.predictors = list(
  #   list("pRate", 1990:1995, "mean"),
  #   list("pRate", 1996:1998, "mean")
  # ),
  predictors.op = "mean",
  time.predictors.prior = 1990:1998,
  dependent = "pRate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = u_20_codes$Code[u_20_codes$Country =="England and Wales"],
  controls.identifier = u_20_codes$Code[u_20_codes$Country !="England and Wales"],
  time.optimize.ssr = 1990:1998,
  time.plot = 1990:2013
)


md_u20_scot <- predvalues_synth(dp_u20_scot)
gg_synth(md = md_u20_scot)

(gr_u20_scot <- md_u20_scot %>% 
    ggplot(aes(Year, Rate, col = Group, linetype = Group)) +
    geom_line(size = 1.5) +
    theme_sphsu_light() +
    ylab("Under-20 pregnancy rate (per 1,000 women)") +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
    scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
    geom_vline(xintercept = 1998.5, linetype = "dotted"))
  
export::graph2ppt(gr_u20_scot, "Under 20 pregnancy rates - including Scotland.ppt", height = 6, width = 9.5)


st_u20_scot <- synth.tab(so_u20_scot, dp_u20_scot)  # summarising results

st_u20_scot$tab.w  # weight
st_u20_scot$tab.v  # weight


# ** U20 special predictors synth -----------------------------------------------------------------------------------

it_U20_sp <- testSynthInterations(Margin.ipop=.005, Sigf.ipop=7, Bound.ipop=6)
it_U20_sp %>% ggplot(aes(iteration, mspe, col = factor(groups))) + geom_point()
it_U20_sp %>% filter(groups == 3) %>% arrange(mspe)
sp_U20_Sp <- it_U20_sp$sPred[it_U20_sp$iteration==19][[1]]

dp_u20_sp <- dataprep(
  foo = data.frame(synthData_U20 %>% filter(Year > 1989)),
  special.predictors = sp_U20_Sp,
  predictors.op = "mean",
  time.predictors.prior = 1990:1998,
  dependent = "pRate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = u_20_codes$Code[u_20_codes$Country =="England and Wales"],
  controls.identifier = u_20_codes$Code[u_20_codes$Country !="England and Wales"],
  time.optimize.ssr = 1990:1998,
  time.plot = 1990:2013
)


md_u20_sp <- predvalues_synth(dp_u20_sp)
gr_U20_sp <- gg_synth(md = md_u20_sp, post = TRUE, mspe = mspe_limit_u20_sp)

gr_U20_sp + labs(title = sPredText(dp_u20_sp))

ggsave("graphs/Under 20 pregnancy rates - special predictors.png")  

# export::graph2ppt(gr_u20_sp, "graphs/Under 20 pregnancy rates - special predictors.ppt", height = 6, width = 9.5)

st_u20_sp$tab.w  # weight
st_u20_sp$tab.v  # weight

mspe_limit_u20_sp <- so_u20_sp$loss.v[1]
  
pl_U20_sp <- generatePlacebos(synthData_U20 %>% filter(Year>1989), time.plot = 1990:2013, special.predictors = sp_U20_Sp, dependent = "pRate")

md_u20_sp %>% 
  spread(Group, Rate) %>% 
  mutate(Gap = Treated - Synthetic) %>% 
  ggplot(aes(Year, Gap)) +
  geom_line(data = pl_U20_sp %>% filter(pre_mspe < 5*mspe_limit_u20_sp), aes(group = Country), col = "grey") +
  geom_line(col = sphsu_cols("Thistle", names = FALSE), size = 2) +
  theme_minimal() +
  geom_vline(xintercept = 1998.5, linetype = "dotted") +
  labs(title = sPredText(dp_u20_sp), subtitle = paste0("MSPE pre-intervention: ", round(mspe_limit_u20_sp, 3), " (controls <5*MSPE)"))

ggsave("graphs/Under-20 rates - inc Scot+NI - placebo test.png")

# ** U20 basic synth excluding Scotland -----------------------------------------------------------------------------------

synthData_U20_b <- synthData %>%  # Add UK GDP to England as estimate
  filter(Country=="United Kingdom", agegrp == "Under 20", Year < 2014) %>% 
  mutate(Country = "England and Wales") %>% 
  select(Country, Year, GDPperCap) %>% 
  left_join(synthData_U20 %>% filter(Country == "England and Wales") %>% select(-GDPperCap), by = c("Country", "Year")) %>% 
  bind_rows(synthData_U20 %>% filter(Country != "England and Wales")) %>%
  filter(Country != "Scotland",
         Country != "Estonia",
         Country != "Lithuania", 
         Country != "Hungary", 
         Country != "Slovenia",
         Year > 1989)

controls_U20_b <- synthData_U20_b %>%  filter(Country != "England and Wales") %>%
  select(Code) %>% 
  unique() %>% 
  pull()

dp_u20_simple <- dataprep(
  foo = data.frame(synthData_U20_b),
  # predictors = c("pRate"),
  special.predictors = list(
    list("pRate", 1990:1995, "mean"),
    list("pRate", 1996:1998, "mean")
  ),
  predictors.op = "mean",
  time.predictors.prior = 1990:1998,
  dependent = "pRate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls_U20_b,
  time.optimize.ssr = 1996:1998,
  time.plot = 1990:2013
)

so_u20_simple <- synth(dp_u20_simple)  # synth output
st_u20_simple <- synth.tab(so_u20_simple, dp_u20_simple)  # summarising results
st_u20_simple$tab.w  # weight


md_u20_simple <- predvalues_synth(dp_u20_simple)

gg_synth(md = md_u20_simple)

(gr_u20_simple <- md_u20_simple %>% 
    ggplot(aes(Year, Rate, col = Group, linetype = Group)) +
    geom_line(size = 1.5) +
    theme_sphsu_light() +
    ylab("Under-20 pregnancy rate (per 1,000 women)") +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
    scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
    geom_vline(xintercept = 1998.5, linetype = "dotted"))

printCoefficients(md = md_u20_simple)

export::graph2ppt(gr_u20_simple, "Under 20 pregnancy rates.ppt", height = 6, width = 9.5)



# ** U20 basic synth with GDPpercap -----------------------------------------------------------------------------------



dp_u20_gdp <- dataprep(
  foo = data.frame(synthData_U20_b),
  predictors = c("GDPperCap"),
  special.predictors = list(
    list("pRate", 1990:1995, "mean"),
    list("pRate", 1996:1998, "mean")
  ),
  predictors.op = "mean",
  time.predictors.prior = 1990:1998,
  dependent = "pRate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls_U20_b,
  time.optimize.ssr = 1996:1998,
  time.plot = 1990:2013
)

so_u20_gdp <- synth(dp_u20_gdp)  # synth output
st_u20_gdp <- synth.tab(so_u20_gdp, dp_u20_gdp)  # summarising results
st_u20_gdp$tab.w  # weight
st_u20_gdp$tab.v  # weight

md_u20_gdp <- predvalues_synth(dp_u20_gdp)

(gr_u20_gdp <- md_u20_gdp %>% 
    ggplot(aes(Year, Rate, col = Group, linetype = Group)) +
    geom_line(size = 1.5) +
    theme_sphsu_light() +
    ylab("Under-20 pregnancy rate (per 1,000 women)") +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
    scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
    geom_vline(xintercept = 1998.5, linetype = "dotted"))

printCoefficients(md_u20_gdp)

export::graph2ppt(gr_u20_gdp, "Under 20 pregnancy rates with GDP as predictor.ppt", height = 6, width = 9.5)



# ** U20 basic synth with all variables -----------------------------------------------------------------------------------

synthData_U20_a <- synthData %>% 
  filter(agegrp == "Under 20", Country != "England and Wales") %>% 
  mutate(Country = as.character(Country)) %>% 
  select(Country, Year, MobilePhones, UrbanPop, MF_ratio) %>% 
  mutate(Country = ifelse(Country == "United Kingdom", "England and Wales", Country)) %>% 
  right_join(synthData_U20_b, by = c("Country", "Year")) %>% 
  filter(Country != "New Zealand")

controls_U20_a <- synthData_U20_a %>%  filter(Country != "England and Wales") %>%
  select(Code) %>% 
  unique() %>% 
  pull()

dp_u20_all <- dataprep(
  foo = data.frame(synthData_U20_a),
  predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"),
  special.predictors = list(
    list("pRate", 1990:1995, "mean"),
    list("pRate", 1996:1998, "mean")
  ),
  predictors.op = "mean",
  time.predictors.prior = 1990:1998,
  dependent = "pRate",
  unit.variable = "Code",
  unit.names.variable = "Country",
  time.variable = "Year",
  treatment.identifier = "England and Wales",
  controls.identifier = controls_U20_a,
  time.optimize.ssr = 1996:1998,
  time.plot = 1990:2013
)

so_u20_all <- synth(dp_u20_all)  # synth output
st_u20_all <- synth.tab(so_u20_all, dp_u20_all)  # summarising results
gaps.plot(so_u20_all, dp_u20_all)  # summarising results
st_u20_all$tab.w  # weight
st_u20_all$tab.v  # weight

md_u20_all <- predvalues_synth(dp_u20_all)
gg_synth(md = md_u20_all)

(gr_u20_all <- md_u20_all %>% 
    ggplot(aes(Year, Rate, col = Group, linetype = Group)) +
    geom_line(size = 1.5) +
    theme_sphsu_light() +
    ylab("Under-20 pregnancy rate (per 1,000 women)") +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
    scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
    geom_vline(xintercept = 1998.5, linetype = "dotted"))

printCoefficients(md_u20_all)

export::graph2ppt(gr_u20_all, "Under 20 pregnancy rates with multiple predictors.ppt")



# comparing with/without gdp ---------------------------------------------------------------------------------

# diff_simple_gdp <- left_join(md_u20_gdp, md_u20_simple, by = c("Year", "Group")) %>% filter(Group == "Synthetic")
# 
# weights_simple_gdp <- left_join(st_u20_gdp$tab.w, st_u20_simple$tab.w, by = c("unit.names", "unit.numbers")) %>% 
#   select(Code = unit.numbers, Country = unit.names, gdp.weights = w.weights.x, simple.weights = w.weights.y) %>% 
#   mutate(Country = as.character(Country))
# 
# synthData_U20_b %>% filter(Year == 2005, Country != "England and Wales") %>% 
#   left_join(weights_simple_gdp, by = c("Code", "Country")) %>% 
#   summarize(gdp.mean = sum(pRate * gdp.weights),
#          simple.mean = sum(pRate * simple.weights))
# 
# 
# md_u20_gdp %>% 
#   ggplot(aes(Year, Rate, col = Group, linetype = Group)) +
#   geom_line(size = 1.5) +
#   geom_line(data = md_u20_simple %>% filter(Group == "Synthetic"), aes(col = "Synthetic, without GDP", linetype = "Synthetic, without GDP"), size = 1) +
#   theme_sphsu_light() +
#   ylab("Under-20 pregnancy rate (per 1,000 women)") +
#   theme(legend.title = element_blank(),
#         panel.grid = element_blank(),
#         axis.line = element_blank()) +
#   scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Synthetic, without GDP" = "dashed", "Treated" = "solid")) +
#   scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Synthetic, without GDP" = sphsu_cols("Leaf", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
#   geom_vline(xintercept = 1998.5, linetype = "dotted")

# Getting differences in trend -------------------------------------------------------------------------------

gg_synth(dp_rateSp)

Eng_synth <- predvalues_synth(dp_rateSp) 

Eng_synth %>%
  gather("Cond", "Value", -Year) %>% 
  filter(Year > 1998) %>% 
  mutate(Time = Year-1998) %T>%
  {print(ggplot(., aes(Year, Value, group = Cond, col = Cond)) +
           geom_smooth(method = "lm", se = FALSE) + 
           theme_sphsu_light())} %>%
  lm(Value ~ Time + Cond + Cond:Time,.) -> model_Engsynth

summary(model_Engsynth)$coefficients %>% as_tibble() %>% 
  mutate(Coefficient = row.names(summary(model_Engsynth)$coefficients)) %>% 
  select(5, 1:2) %>% 
  mutate(hiCI = Estimate + 1.96 * `Std. Error`,
         lowCI = Estimate - 1.96 * `Std. Error`)


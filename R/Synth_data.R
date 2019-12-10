library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(purrr)

# A script for importing and tidying all data ----------------------------------------------------------------


# ** Data outside Europe -------------------------------------------------------------------------------------

altpops <- read_xlsx("Downloaded data files/population_total.xlsx")
pop_perc <- read_xlsx("Downloaded data files/pop_female_perc.xlsx")
GDP_NZ <- read_xlsx("Downloaded data files/GDPdata.xlsx", sheet = "Data", skip = 3) %>% 
  filter(`Country Name` == "New Zealand") %>% 
  select(Country = `Country Name`, `1985`:`2017`) %>% 
  gather("Year", "GDPperCap", -1) %>% 
  mutate(Year = as.numeric(Year))

GDP_lith <- read_csv("Downloaded data files/Lith-1990-1999-gdp.csv")

hung_mod <- read_xlsx("Downloaded data files/GDPdata.xlsx", sheet = "Data", skip = 3) %>% 
  filter(`Country Name` == "Hungary") %>% 
  select(Country = `Country Name`, `1991`:`1995`) %>% 
  gather("Year", "GDPperCap", -1) %>% 
  lm(GDPperCap ~ as.numeric(Year), data = .)

# **** New Zealand ----------------------------
NZ_import_rates <- read_csv("Downloaded data files/NZ_totalpregrates.csv", skip=2, col_names=c("Year", "Under 15", "Under 20"))[1:26,]

totpop_NZ <- altpops %>%
  filter(country == "New Zealand") %>% 
  select(Country = country, `1990`:`2017`) %>% 
  gather("Year", "totpop", -1)

u_20_pop <- pop_perc %>% filter(country == "New Zealand") %>% select(Country = country, `1990`:`2017`) %>%
  gather("Year", "pop_perc", -1) %>%
  mutate(pop_perc = ifelse(is.na(.$pop_perc), mean(pop_perc, na.rm = TRUE), pop_perc)) %>%
  left_join(totpop_NZ, by = c("Country", "Year")) %>%
  mutate(sumPops = totpop * pop_perc / 100, agegrp = "Under 20") %>%
  select(Country, Year, agegrp, sumPops)

nz_est_1990_01 <- NZ_import_rates %>% 
  filter(Year < 1994) %>% 
  select(Year, totalPregs = `Under 20`) %>% 
  mutate(Year = c("1990", "1991"),
         Country = "New Zealand",
         agegrp = "Under 20") %>% 
  left_join(u_20_pop, by = c("Country", "Year", "agegrp"))

NZ_totalrates <- NZ_import_rates %>%
  select(Year, `Under 20`) %>% 
  gather("agegrp", "totalPregs", -1) %>% 
  left_join(u_20_pop, by = c("Year", "agegrp")) %>% 
    bind_rows(nz_est_1990_01) %>% 
  mutate(pRate = 1000*totalPregs/sumPops,
         Year = as.numeric(Year),
         Code = "NZL") %>% 
  left_join(GDP_NZ) %>% 
  arrange(Year)

# ** Under-18 data -------------------------------------------------------------------------------------------


ccodes_new <- read.csv("country_codes_complete.csv", stringsAsFactors = FALSE, header = TRUE)
synthData <- read.csv("Downloaded data files/SynthData in progress.csv", stringsAsFactors = FALSE) 
synthData[synthData$Country=="Hungary" & synthData$Year == 1990, "GDPperCap"] <- as.numeric(predict(hung_mod, newdata = tibble(Year = 1990)))

# replace England and Wales missing data with UK totals

synthData[synthData$Country=="England and Wales",c("GDPperCap", "MobilePhones", "UrbanPop")] <- synthData[synthData$Country=="United Kingdom",c(8,10,11)]

synthData[synthData$Country == "Spain" &
            synthData$agegrp == "Under 18" & 
            synthData$Year == 1985, "MobilePhones"] <- 0

synthData[synthData$Year %in% 1990:1999 & synthData$Country=="Lithuania",c("Year","GDPperCap")] <-
synthData[synthData$Year %in% 1990:1999 & synthData$Country=="Lithuania","Year"] %>%
  tibble(Year = .) %>% 
  left_join(GDP_lith, by = "Year")


synthData_u18 <- synthData %>%
  filter(agegrp == "Under 18",
         !Country %in% c("United Kingdom", "Croatia", "Bulgaria", "Canada"),
         Year < 2014) %>%  # countries and years with missing data
  arrange(Country, Year) %>% 
  mutate(Country = as.character(Country),
         Code = as.numeric(factor(Code, ordered = TRUE))) %>% 
  select(Code, Country, Year, rate, GDPperCap, MF_ratio, MobilePhones, UrbanPop) %>% 
  data.frame()

synthData_u18[which(synthData_u18$Country=="Poland" & synthData_u18$Year==2001),"rate"] <- 
  mean(synthData_u18[which(synthData_u18$Country=="Poland" & synthData_u18$Year==2000),"rate"],
       synthData_u18[which(synthData_u18$Country=="Poland" & synthData_u18$Year==2002),"rate"])


u_18_ccodes <- synthData_u18 %>% 
  select(Code, Country) %>% 
  unique()


# ** Under 20 data -------------------------------------------------------------------------------------------
allUKrates_u20 <-
  read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 20")

abortions <- read_csv("Downloaded data files/EHIG_abortions.csv", skip = 25)

US_abortions <- read_csv("Downloaded data files/USA_u20_abortions.csv")

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
  semi_join(synthData, by = c("Code", "Country", "Year", "agegrp")) %>% 
  bind_rows(US_abortions)


synth_data_plus_ab <- right_join(abortions_tidy, synthData, by = c("Code", "Country", "Year", "agegrp"))

synth_data_interp_ab <- synth_data_plus_ab %>% 
  select(Country) %>% 
  unique() %>% 
  pull() %>% 
  map_dfr(., ~interpolateAb(.)) %>% 
  mutate(pRate = 1000*totalPregs/sumPops)

Eng_rates <- allUKrates_u20 %>% 
  filter(Country == "England and Wales") %>% 
  gather("Year", "pRate", -1) %>% 
  mutate(Year = as.numeric(Year))

synth_data_interp_ab <- synth_data_interp_ab %>%
  filter(Country == "England and Wales", agegrp == "Under 20") %>% 
  select(-pRate) %>% 
  left_join(Eng_rates, by = c("Country", "Year")) %>% 
  bind_rows(synth_data_interp_ab %>% filter(Country != "England and Wales" | agegrp != "Under 20"))

Sco_rates <- allUKrates_u20 %>% 
  filter(Country == "Scotland") %>% 
  gather("Year", "pRate", -1) %>% 
  mutate(Year = as.numeric(Year))


synth_data_interp_ab <- synth_data_interp_ab %>%
  filter(Country == "Scotland", agegrp == "Under 20") %>% 
  select(-pRate) %>% 
  left_join(Sco_rates, by = c("Country", "Year")) %>% 
  bind_rows(synth_data_interp_ab %>% filter(Country != "Scotland" | agegrp != "Under 20"))


synth_data_interp_ab <- synth_data_interp_ab %>%
  bind_rows(NZ_totalrates, .)

# Synth for total Under 20 pregnancies 

synthData_u20 <- synth_data_interp_ab %>% 
  filter(agegrp == "Under 20") %>% 
  select(Code, Country, Year, GDPperCap, pRate, rate, MF_ratio, MobilePhones, UrbanPop) %>% 
  filter(!Country %in% c("United Kingdom", "Austria", "Croatia", "Canada", "Northern Ireland", "Bulgaria"),
         Year > 1989,
         Year < 2014) %>% 
  arrange(Country, Year) %>% 
  mutate(Code = as.numeric(factor(Code, ordered = TRUE)))


# filling in missing data
synthData_u20[synthData_u20$Country == "Poland" & synthData_u20$Year == 2001, 'pRate'] <- 
  mean(synthData_u20[[which(synthData_u20$Country == "Poland" & synthData_u20$Year == 2000), 'pRate']],
       synthData_u20[[which(synthData_u20$Country == "Poland" & synthData_u20$Year == 2002), 'pRate']])

synthData_u20[synthData_u20$Country == "Scotland" & synthData_u20$Year %in% 1989:1993, 'pRate'] <- read_csv("Downloaded data files/EstScot_1985_2015.csv") %>%
  filter(Year > 1989, Year < 1994) %>% 
  select(Value) %>% 
  pull()

synthData_u20 <- synthData_u20 %>% 
  select(Code, Country, Year, pRate, rate, GDPperCap, MF_ratio, MobilePhones, UrbanPop) %>% 
  data.frame()



u_20_ccodes <- synthData_u20 %>% 
  select(Country, Code) %>% 
  unique()


# New data - same years and countries, filtering --------------------------

new_exclude <- c("Hungary", "Estonia", "Lithuania", "Poland", "Czechia", "Slovenia", "Austria", "Northern Ireland", "New Zealand")

synthData_u18_filt <- synthData_u18 %>% 
  filter(!(Country %in% new_exclude), Year > 1989)

synthData_u20_filt <- synthData_u20 %>% 
  filter(!(Country %in% new_exclude))

u_18_ccodes_f <- synthData_u18_filt%>% 
  select(Country, Code) %>% 
  unique() %>% 
  arrange(Code)

u_20_ccodes_f <- synthData_u20_filt %>% 
  select(Country, Code) %>% 
  unique() %>% 
  arrange(Code)

save(synthData, synthData_u18_filt, synthData_u20_filt, u_18_ccodes_f, u_20_ccodes_f, file = "Data/synth_data_b.rdata")

# exclusion lists --------------------------------------------------------------------------------------------



exclude_u18_gdp <- synthData_u18 %>% 
  filter(is.na(GDPperCap)) %>% 
  select(Country) %>% 
  unique() %>% 
  pull()

exclude_u18_gdp_1990 <- synthData_u18 %>% 
  filter(Year>=1990) %>% 
  filter(is.na(GDPperCap)) %>% 
  select(Country) %>% 
  unique() %>% 
  pull()

exclude_u18_all <- synthData_u18 %>% 
  filter_all(any_vars(is.na(.))) %>% 
  select(Country) %>% 
  unique() %>% 
  pull()

exclude_u18_all_1990 <- synthData_u18 %>% 
  filter(Year>=1990) %>% 
  filter_all(any_vars(is.na(.))) %>% 
  select(Country) %>% 
  unique() %>% 
  pull()

exclude_u20_gdp <- synthData_u20 %>% 
  filter(is.na(GDPperCap)) %>% 
  select(Country) %>% 
  unique() %>% 
  pull()


exclude_u20_all <- synthData_u20 %>% 
  filter_all(any_vars(is.na(.))) %>% 
  select(Country) %>% 
  unique() %>% 
  pull()

save(
  exclude_u18_gdp,
  exclude_u18_all,
  exclude_u18_gdp_1990,
  exclude_u18_all_1990,
  exclude_u20_gdp,
  exclude_u20_all,
  synthData_u18,
  synthData,
  synthData_u20,
  u_18_ccodes,
  u_20_ccodes,
  file = "Data/synth_data.rdata"
)


# excluding Scotland ------------------------------------------------------




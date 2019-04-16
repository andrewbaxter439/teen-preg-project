name <- "Education Expenditure"

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


dfTidy %>% filter(Country=="United Kingdom") %>% pull(`Education Expenditure`) -> EE
dfTidy %>% filter(Country=="United Kingdom") %>% pull(Year) -> Yr


approx(Yr, EE, n = length(Yr), rule = 2)

$y -> predEE

tibble(Yr, EE, predEE)

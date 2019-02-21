#library(readr)
library(foreach)
library(tidyverse)
library(doParallel)
library(gganimate)
#library(rnaturalearth)
#library(stringr)

allbirths <- read_tsv("birthsRR.txt")
ccodes <- read_tsv("country_codes.txt")
allbirths[["Country"]] <-
  pull(ccodes[match(allbirths[["Code"]], ccodes[["Code"]]), "Country"])
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

allbirths %>%
  mutate(Age=as.numeric(sub("-", "", Age))) %>%
  filter(Age>12 & Age<21) -> teenbirths

# countries with missing data
country_names %>%
  subset(!(country_names %in% allbirths$Country))

# countries included in database, not used here
allbirths$Country %>%
  subset(!(allbirths$Country %in% country_names)) %>%
  unique() 

# find codes not translated
allbirths %>% 
  filter(is.na(Country)) %>%
  group_by(Code) %>%
  summarise(n=n()) 

# Read population file -----------------------------------------------------

readpop <- function(code) {
  filename <- paste("DataAnalysis/Population/",
                    code, ".Population.txt",sep="")
  pop <- read.fwf(filename, widths = c(7,13,20,16,16),sep="", skip=2,
                  header=TRUE)
  pop <- dplyr::mutate(pop, Code=code, Age=as.numeric(as.character(Age)),
                       Year=as.numeric(as.character(Year)))
#  left_join(teenbirths, pop[c(1,2,3,6)], by=c("Year","Age","Code"))
#  assign(paste(code,"_pop", sep=""), pop, .GlobalEnv)
}

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
registerDoSEQ()

allpopsroot <- foreach(Code=unique(teenbirths$Code), .combine=rbind) %dopar% {
  readpop(Code)
}

# Standardise Germany labels - combining east/west pre-1990

allpops <- allpopsroot %>%
  filter(Year<1990, str_detect(Code, "DEU")) %>%
  group_by(Year, Age) %>%
  dplyr::summarise(Female=sum(Female), Male=sum(Male), Total=sum(Total)) %>%
  mutate(Code="DEUTNP") %>%
  bind_rows(allpopsroot) %>%
  filter(Code != "DEUTW" & Code != "DEUTE") %>%
  ungroup()

# Poland gives high/low estimates for 2001 - averaging these

allpops <- allpopsroot %>% 
  filter(Code=="POL", is.na(Year)) %>%
  group_by(Age) %>%
  summarise(Female=mean(Female), Male=mean(Male), Total=mean(Total)) %>%
  mutate(Year=2001, Code="POL") %>%
  right_join(allpops)

# Combine with births -----------------------------------------------------

teenrates <- left_join(teenbirths,
                       allpops[c(1,2,5,6)],
                       by=c("Year","Age","Code")) %>%
  rename(Femalepop = Female)





# Group and calculate -----------------------------------------------------
# have included age 12 births here
# - check ONS guidlines and methods and report appropriately

agegrps<-tibble(Age=factor(c(12:15, 12:17, 12:19)),
                agegrp=c(1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3)) # denominators for rate calc
agecats<-tibble(Age=factor(c(13:15, 15:17, 15:19)),
                agecat=c(1,1,1,2,2,2,3,3,3,3,3)) # numerators for rate calc
agecalcs<-merge(agegrps, agecats)

birthrates <- teenrates %>%
  filter(Year>1984, Country %in% country_names) %>%
  merge(agecalcs) %>%
  group_by(Year, Country, agegrp, agecat) %>%
  summarise(popsum=sum(Femalepop), birthsum=sum(Total)) %>%
  ungroup() %>%
  filter(agegrp==agecat) %>%
  mutate(agecat=factor(agecat,
                       labels = c("Under 16", "Under 18", "Under 20"))) %>%
  mutate(rate=birthsum/popsum)
  
# Graphs of all -----------------------------------------------------

birthrates %>%
  filter(Country!="United Kingdom"& Country!="Bulgaria") %>% # Scot/EngWa/NI seperate
  ggplot(aes(x=Year, y=rate, col=Country, group=Country)) +
    geom_line() +
   xlim(1985,2016) +
#   scale_y_continuous(limits=c(0,0.065)) +
    theme_minimal() +
    facet_wrap(~agecat) +
#  geom_smooth(aes(col=NULL,group=NULL), size=1.5, col="steelblue")
    stat_summary(aes(col=NULL, group=NULL),
                 fun.y=mean, geom="line", size=1.5, col="steelblue") 
#    stat_summary(aes(col=NULL,group=NULL), geom="ribbon", fun.data = mean_sdl, fun.ymin = "Lower", fun.ymax = "Upper", alpha=0.3, fill="steelblue")



# Scotland filer and graph to trial data - not used

allbirths %>%
  mutate(Age=as.numeric(sub("-", "", Age))) %>%
  filter(Age>12 & Age<20 & Country=="Scotland")%>%
  ggplot(aes(x=Year, y=Total, col=factor(Age), group=Age)) +
  geom_line() +
  xlim(1985, NA) +
  theme_minimal()
Scopop<-read_tsv("SCOPop.txt")

allbirths %>%
  mutate(Age=sub("-", "", Age)) %>%
  filter(Age<20 & Country=="Scotland") %>%
  left_join(Scopop[1:3], by=c("Year", "Age")) %>%
  mutate(Age=factor(Age, ordered = TRUE)) -> Scobirthpop

Scobirthpop <- select(Scobirthpop, -c(1,5))

Scobirthpop %>%
  filter(Year>1984) %>%
  merge(agecalcs) %>%
  group_by(Year, agegrp, agecat) %>%
  summarise(popsum=sum(Female), birthsum=sum(Total)) %>%
  ungroup() %>%
  filter(agegrp==agecat) %>%
  mutate(agecat=factor(agecat,
                       labels = c("Under 16", "Under 18", "Under 20"))) %>%
  mutate(rate=birthsum/popsum) %>%
  ggplot(aes(x=Year, y=rate, col=agecat, group=agecat))+
  geom_line()+
  theme_minimal()
         
         
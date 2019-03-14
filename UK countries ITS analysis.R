library(tidyverse)
library(readxl)
library(broom)
library(nlme)
library(car)

testAutocorr <- function(data, model) {
  print(dwt(model, max.lag=10, alternative="two.sided"))
  par(cex=0.7, mai=c(0.1,0.1,0.2,0.1))
  par(fig=c(0.03,1,0.8,1))
  plot(data$Time[1:25], residuals(model)[1:25])
  
  par(fig=c(0.03,0.5,0.05,0.75), new=TRUE)
  acf(residuals(model))
  
  par(fig=c(0.55,1,0.05,0.75), new=TRUE)
  acf(residuals(model),type='partial')
}

# Import Scotland data ---------------------------------------------------------------------------------------

Scotland.conc <- read_xls("Downloaded data files/ScotISD2018 report.xls", sheet = "Table1",
                          range = "B33:P55", col_names = FALSE) %>% 
  select(Year = '..1',
         '<16' = '..11',
         '<18' = '..13',
         '<20' = '..15')

# Scotland births to estimate pregnancies pre-1994
# Data from NRS

Scot.births <- read_xlsx("Downloaded data files/Scotland births.xlsx", range = "AQ4:BW16",
                          col_names = TRUE)[c(3:10,12),] %>% 
  select(Age = '..33', 1:32) %>% 
  mutate_all(as.numeric)

# Create age calculation groups ### NOW DEFUNCT

birthAgeGrps <- tibble(Age=factor(c(12:15, 12:17, 12:19)),
                       agegrp=factor(c(1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3),
                                     labels=c("Under 16", "Under 18", "Under 20")))  # Three ranges of births

sumScotBirths <- Scot.births %>% 
  gather("Year", "Total", -1) %>% 
  mutate(Year = as.numeric(Year))%>% 
  merge(birthAgeGrps) %>% 
  group_by(Year, agegrp) %>% 
  summarise(sumBirths = sum(Total)) # create summary

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



# Simple reading of conception rates from combined data ------------------------------------------------------

all.UK.rates <- read_xlsx("Conception rates by age and country.xlsx", sheet = "Under 18")

all.UK.rates %>% filter(Country=="Scotland"|
                        Country=="England"|
                        Country=="Wales") %>% 
  gather("Year", "Value", -1) %>% 
  mutate(Category = ifelse(Year<1999, 0,
                ifelse(Year<2007, 1, 2)),
         Year=as.numeric(Year)) %>% 
  group_by(Country, Category) %T>% 
  {print(group_map(., ~tidy(lm(Value ~ Year, data=.))) %>% 
           arrange(Category, term, Country))} %>% 
  {ggplot(., aes(x=Year, y=Value, group=interaction(Category, Country), col=Country)) +
  geom_point() +
  geom_smooth(method="lm")}
  

# EngMod - England data in ITS with break at 1999 ------------------------------------------------------------

EngData <- all.UK.rates %>% filter(Country=="England") %>% 
  select(Country, '1992':'2016') %>% 
  gather("Year", "Value", -1) %>% 
  mutate(Cat1 = ifelse(Year<1999, 0, 1),
         Cat2 = ifelse(Year<=2007, 0, 1),
         Year=as.numeric(Year),
         Time = 1:nrow(.),
         Trend1 = ifelse(Cat1==0, 0, 1:nrow(.)-nrow(filter(., Cat1==0))),
         Trend2 = ifelse(Cat2==0, 0, 1:nrow(.)-nrow(filter(., Cat2==0))))

EngData %>% ggplot(aes(x=Year, y=Value, group=Cat1)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

EngMod <- lm(Value ~ Time + Cat1 + Trend1, data = EngData)  
summary(EngMod)

# Test for autocorrelation

plot(EngData$Time, residuals(EngMod))
dwt(EngMod, max.lag=10, alternative="two.sided")

par(mfrow=c(1,2))

# Produce plots
acf(residuals(EngMod))
acf(residuals(EngMod),type='partial')


# EngMod2 - proposing new effect at Y=2007 -------------------------------------------------------------------

EngData %>% ggplot(aes(x=Year, y=Value, group=interaction(Cat1, Cat2))) +
  geom_point() +
  geom_smooth(method="lm")

EngMod2 <- lm(Value ~ Time + Cat1 + Trend1 + Cat2 + Trend2, data = EngData)  
summary(EngMod2)

# Test for autocorrelation

plot(EngData$Time, residuals(EngMod2))
dwt(EngMod2, max.lag=10, alternative="two.sided")

par(mfrow=c(1,2))

# Produce plots
acf(residuals(EngMod2))
acf(residuals(EngMod2),type='partial')

EngMod2_p3 <- gls(Value ~ Time + Cat1 + Trend1 + Cat2 + Trend2,
                  data=EngData,
                  correlation = corARMA(p=3, form=~Time),
                  method = "ML")
summary(EngMod2_p3)

EngMod2_p3 %>% update(correlation=corARMA(p=4, form=~Time)) %>% 
  anova(EngMod2_p3)

EngMod2_p3 %>% update(correlation=corARMA(p=3, q=1, form=~Time)) %>% 
  anova(EngMod2_p3)


# Preliminary analysis - Eng v Scot --------------------------------------------------------------------------
## Setting up data
 EngScotContro <-
  all.UK.rates %>% filter(Country=="Scotland"|
                          Country=="England") %>% 
  gather("Year", "Value", -1) %>%
  filter(Year>1991) %>%
  arrange(Country) %>%
  mutate(England = ifelse(Country=="England", 1, 0),
         Year=as.numeric(Year),
         Time=Year-min(Year)+1,
         Cat1 = ifelse(Year<1999, 0, 1),
         Cat2 = ifelse(Year<=2007, 0, 1),
         Trend1 = ifelse(Cat1==0, 0, Year-1998),
         Trend2 = ifelse(Cat2==0, 0, Year-2007)) %>% 
  mutate_at(., colnames(.)[5:9], list(Eng = ~.*England))  # Potentially not necessary

## Comparing for pre-post 1999 --------------------------------------------

EngScotContro %T>% 
  {print(ggplot(., aes(Year, Value, group=interaction(Country, Cat1), col=Country))+
  geom_point() + geom_smooth(method="lm", se=FALSE))} %>% 
  lm(Value ~ Time + England + Time*England + Cat1 + Trend1 + Cat1*England + Trend1*England, data=.) %>% 
  assign("modScot99",., envir = .GlobalEnv) %>% 
  print(.$coefs)
#  summary()

testAutocorr(EngScotContro, modScot99)
  
## Comparing for pre-post 2007

EngScotContro %T>% 
{print(ggplot(., aes(Year, Value, group=interaction(Country, Cat2), col=Country))+
         geom_point() + geom_smooth(method="lm"))} %>% 
  lm(Value ~ Time + England + Time*England + Cat2 + Trend2 + Cat2*England + Trend2*England, data=.) %>% 
  summary()

## Comparing for thee stages, split at 1999 and 2007

EngScotContro %T>% 
{print(ggplot(., aes(Year, Value, group=interaction(Country, Cat1, Cat2), col=Country))+
         geom_point() + geom_smooth(method="lm"))} %>% 
  lm(Value ~ Time + 
       England + 
       Time*England + 
       Cat1 + 
       Trend1 + 
       Cat1*England + 
       Trend1*England +
       Cat2 +
       Trend2 +
       Cat2*England +
       Trend2*England,
     data=.) %>% 
  summary()

library(tidyverse)
library(readxl)

filename <- "England vs Scotland 1992-2016.xlsx"
predYr = c(1999:2016)


# First part not working -------------------------------------------------------------------------------------



#  Creating a function
# diffFromPred <- function(filename, predYr = c(1999:2016)) {
modelCoefs <- read_xlsx(paste0("model outputs/", filename), 
                       sheet = "model",
                       col_types = c("text",
                                     "numeric",
                                     "numeric",
                                     "numeric",
                                     "numeric"
                                     ),
                       trim_ws = TRUE)

modeldata <- read_xlsx(paste0("model outputs/", filename), 
                       sheet = "data")

cfdata <-  read_xlsx(paste0("model outputs/", filename), 
                     sheet = "counterfactual")


beta <- modelCoefs$Value
SE <- modelCoefs$Std.Error

predDiffs <- 
  cfdata %>% 
  left_join(modeldata, by = c("Time", "England")) %>% 
  transmute(
    Year = Year,
    Difference = Value - Predict,
    UpperCI = Value - HiCI,
    LowerCI =   Value - lowCI
  ) %>% 
  filter(Year == predYr)

# predictorVar <- cfdata[which(cfdata[,'Time']==)]

modeldata %>% 
  filter(Country=="England", Year %in% predYr) %>% 
  select(-Country, -Value) %>% 
  mutate(EngVal = sum(unlist(.[2:12])))
           # rowSums(.[2:12]))#  * beta[2:12]) + beta[1])
           # beta[1] + 
           # beta[2] * Time +
           # beta[3] * Time_Eng +
           # beta[4] * Cat1 +
           # beta[5] * Trend1 +
           # beta[6] *

# }

coefs <- beta
colname <- "predicted"

ModeldataEng <- modeldata %>% 
  filter(Country=="England") %>% 
  select(-Country) 
  

sumCoefs <- function(modeldata, coefs, colname) {         
  filtereddata <- modeldata

  modeldata[,colname] <- NA
  i <- 1
for (i in 1:nrow(filtereddata)) {
  modeldata[i,colname] <- sum(filtereddata[i,3:13] * coefs[2:12]) + coefs[1]
  
}
  
  return(modeldata)
}

mddatatrial <- sumCoefs(ModeldataEng, beta, "predicted")


# Second part not working ---------------------------------------------------------------------------

mm <- model.matrix(~ Time +
                     England +
                     Time_Eng +
                     Cat1 +
                     Trend1 +
                     Cat1_Eng +
                     Trend1_Eng +
                     Cat2 +
                     Trend2 +
                     Cat2_Eng +
                     Trend2_Eng,
                   data = modeldata %>% filter(Country == "England"))

mm %*% beta


# Importing means and SDs from output data function --------------------------------------------------------------

calcDiffs <- function(filename, predYr = c(1999:2016)) {

modeldata <- read_xlsx(paste0("model outputs/", filename), 
                       sheet = "data")

cfdata <-  read_xlsx(paste0("model outputs/", filename), 
                     sheet = "counterfactual")

df <- tibble(Year = predYr) %>% 
  left_join(modeldata %>% filter(Country == "England") %>% select(Year, Time, EngPred = Predict, EngSE = se), by = "Year") %>% 
  left_join(cfdata %>% select(Time, CfPred = Predict, CfSE = se), by = "Time")
  
df %>% mutate(Diff = EngPred - CfPred,
              SEcomb = sqrt(EngSE^2 + CfSE^2),
              lowCI = Diff - 1.96 * SEcomb,
              hiCI = Diff + 1.96 * SEcomb) %>% 
  return()

}

calcDiffs("England vs Scotland 1992-2016.xlsx", c(2007, 2016))
calcDiffs("England vs Wales 1992-2016.xlsx", c(2007, 2016))

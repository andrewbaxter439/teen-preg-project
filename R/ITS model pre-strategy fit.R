library(tidyverse)
library(readxl)

output_tables <- map(excel_sheets("Data/its_outputs.xlsx"), ~
      read_xlsx("Data/its_outputs.xlsx", sheet = .x))


output_tables %>% 
  map(~ filter(.x, Year < 1999, 
               Country == "England") %>% 
        summarise(mspe = Metrics::mse(Value, Predict),
                  rsq = cor(Value, Predict) ^2))

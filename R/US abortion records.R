library(tidyverse)

US.abort.root <- read_tsv("Downloaded data files/USA/usabirthsabortpoptsv.txt", col_names = TRUE)
head(US.abort.root)
str(US.abort.root)
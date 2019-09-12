library(tidyverse)

us_births<-read_tsv('Downloaded data files/USA/usabirthtsv.txt', col_names = FALSE)
us_births<-us_births[,c(1,3:6)]
us_births<-t(us_births)
colnames(us_births)<-as.character(c('age', us_births[1,2:ncol(us_births)]))
us_births<-us_births[-1,]
us_births<-data.frame(us_births[,c(1, rev(2:ncol(us_births)))])

us_data<-read_tsv('Downloaded data files/USA/usabirthsabortpoptsv.txt', col_names = TRUE)
us_data <- us_data %>% mutate(pop=pop*1000, Year=as.Date.numeric(Year))
us_data %>%ggplot(aes(x=Year, y=pregrate, colour=age))+geom_line()+theme_light() +
 scale_x_continuous(expand=c(0,0))

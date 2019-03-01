---
title: "RMD practice - again"
author: "Andrew Baxter"
date: "1 March 2019"
output: 
  word_document: 
    keep_md: yes
---






```r
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
```

```
## # A tibble: 18 x 7
## # Groups:   Country, Category [9]
##    Country  Category term         estimate std.error statistic      p.value
##    <chr>       <dbl> <chr>           <dbl>     <dbl>     <dbl>        <dbl>
##  1 England         0 (Intercept)  -1.45e+3  589.       -2.47        5.69e-2
##  2 Scotland        0 (Intercept)  -1.85e+3  807.       -2.30        1.05e-1
##  3 Wales           0 (Intercept)  -2.62e+3  735.       -3.56        1.61e-2
##  4 England         0 Year          7.50e-1    0.295     2.54        5.19e-2
##  5 Scotland        0 Year          9.50e-1    0.404     2.35        1.00e-1
##  6 Wales           0 Year          1.34e+0    0.369     3.63        1.50e-2
##  7 England         1 (Intercept)   1.09e+3  121.        9.00        1.05e-4
##  8 Scotland        1 (Intercept)   6.23e+1  398.        0.157       8.81e-1
##  9 Wales           1 (Intercept)   1.53e+3  416.        3.67        1.05e-2
## 10 England         1 Year         -5.21e-1    0.0603   -8.65        1.32e-4
## 11 Scotland        1 Year         -1.07e-2    0.199    -0.0539      9.59e-1
## 12 Wales           1 Year         -7.39e-1    0.208    -3.56        1.20e-2
## 13 England         2 (Intercept)   5.39e+3  180.       30.0         1.65e-9
## 14 Scotland        2 (Intercept)   5.53e+3  216.       25.6         5.86e-9
## 15 Wales           2 (Intercept)   5.54e+3  192.       28.9         2.24e-9
## 16 England         2 Year         -2.67e+0    0.0893  -29.8         1.72e-9
## 17 Scotland        2 Year         -2.73e+0    0.107   -25.4         6.12e-9
## 18 Wales           2 Year         -2.74e+0    0.0954  -28.7         2.35e-9
```

```
## Warning: Removed 23 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 23 rows containing missing values (geom_point).
```

![](practice_again_files/figure-docx/graph-1.png)<!-- -->


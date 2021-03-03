<!-- badges: start -->
[![DOI](https://zenodo.org/badge/171463624.svg)](https://zenodo.org/badge/latestdoi/171463624)
[![OSF](https://img.shields.io/static/v1?label=OSF&message=osf.io%2F8u9jp&color=15A5EB&link=https://osf.io/8u9jp/&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAOxAAADsQBlSsOGwAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAALrSURBVDiNjZRbaFRXFIb/fWaS1k5IpJiQeoGmFodesOIkRLBeUNoIQpW2AaFvgaJtRaUPFbwRrZdGQYtP3uJrKfpSVDLBNIH2QW1UmubWUqs+VM3MJMHEZGYyM+d8ffCccTIZnf5PZ6/zr3+v9a+9t1QEwGfAfSAGHCjGLya2CEgzHY0vyrFyki1gO9ABnAXmSloqye8gZcjm1APVwGmX+zXgK1TNjrxKeoBg7+N0+oPOYd6/GuPnoSTAp0B3HnfnjAolfZS3x+LLQ4mK/b3j10dTjhI2Ov7nxMMLdyf/klSbx91QSPB+LiNpo6MDk2fuxTM/ebGhpH3l+7uJ1riN8nCvkOA+SX2SNGWjwwMTiqcJ3hqrOmGMGpGakuNVW6ds3jrY90SJZ6KDknZ7C+P6t1JSSNKttkgi1dI/eWoiTdAYs+tmQ+WJ3FJCbdFtMmoJ+Kw737xdsXn9XL8tqU7SDWNMt9zJAnBteMrZ+MtIf204emh1F/78vrJoxh8Kx/Z+/Ovo77+NpBw33QE2G+CRpOoM0trOYU265wOjTbcbqn4spFcbjnyCzEVJKvMbdayZI7+RJP1rSZrhcFFglXifDpJDVsJvSfpOEn4jtSwpZ8ErvkEjHV44VnnxeXo1T+ZcMMYcmzfLunPovXKVWsb7dcwbynI9Nba742E6fmDw8cl4RouQ2XN7XeXZae22R5tARwI+/b3rndnbGl4rmS1pmaRrxpiuZ13APPd2kMg47OkZIxSOTgisUHtkY6gt2qhmrFA4Or73j3GSGW8W9LjXVJJ7bFzBVklN3jppow+7RgfiGecHGX3rhlvKSqz17ateffclXzZVks4ZYz6Xph/smlzGyz6jL4OBLRjqvJhltOyrNwNf5IlJ0hszjH7O42BdepAIr+iIsfxqjCsPkoNAKdCbx91eSNB7vjqBVtfTMsBJ2TBlZz2rB+YD54EutxBrhmAhAD5gJKeSDPD6/0p+geg64B8gAmwpxv8Pzpwmb9gDIfgAAAAASUVORK5CYII=)](https://osf.io/8u9jp/)
[![Docker](https://img.shields.io/docker/cloud/build/andybaxter/teen-preg-project?logo=docker)](https://hub.docker.com/r/andybaxter/teen-preg-project)
[![License](https://img.shields.io/github/license/andrewbaxter439/teen-preg-project)](https://github.com/andrewbaxter439/teen-preg-project/blob/master/LICENSE)
<!-- badges: end -->

# Project - Natural Experimental testing of teenage pregnancy rates and causes

Code used in the project evaluating England's Teenage Pregnancy Strategy.

## Repeating analyses

The file `Synth-iterations-report.rmd` will produce outputs for Synthetic Control analyses. The Interrupted Time Series analyses can be reproduced [here](https://andybaxter.shinyapps.io/teen_preg_uk_its/) using a Shiny app (repo for Shiny app [here](https://github.com/andrewbaxter439/ITS_shinyapp))

To get started, clone the repository, open `teen-preg-project.Rproj` and run `renv::restore()` in Rstudio. Open `Synth-iterations-report.rmd` and run all chunks to reproduce synthetic control analyses.

The `R` folder contains all code developed throughout the project.

## Data Sources
(Source data is freely available from all sources but cannot be redistributed here. Compiled data included as `.rdata` files)

 - [The Human Mortality Database](https://www.mortality.org/) (Requires registration; accessed 9 Apr 2019)
 - [The Human Fertility Database](https://www.humanfertility.org/) (Requires registration; accessed 25 March 2019)
 - [Statistics New Zealand](http://stats.govt.nz) (accessed 19 Feburary 2019) - tables VSB004AA and ABN005AA
 - [World Bank Open Data](https://data.worldbank.org/) (accessed 23 April 2019)
 - [European Health Information Gateway - Health for All Explorer](https://gateway.euro.who.int/en/hfa-explorer/) (accessed 9 September 2019)
 - [Guttmacher Institute - Pregnancies, Births and Abortions Among Adolescents and Young Women in the United States, 2013](https://www.guttmacher.org/report/us-adolescent-pregnancy-trends-2013) (accessed 17 January 2019)
 - [Public spending on education - OECD Data](https://data.oecd.org/eduresource/public-spending-on-education.htm) (accessed 21 October 2019)

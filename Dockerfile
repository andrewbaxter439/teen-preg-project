FROM rocker/tidyverse
LABEL maintainer="Andrew Baxter <a.baxter.1@research.gla.ac.uk>"

ENV RENV_VERSION 0.13.0
RUN apt-get update && apt-get -y --no-install-recommends install libudunits2-dev libgdal-dev
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /project
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

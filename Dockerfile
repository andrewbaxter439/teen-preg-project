FROM rocker/tidyverse
LABEL maintainer="Andrew Baxter <a.baxter.1@research.gla.ac.uk>"

RUN R -e "source(renv/activate.R);
  renv::restore();"

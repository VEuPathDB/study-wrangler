FROM rocker/tidyverse:4

## Set a default user. Available via runtime flag `--user rserve`
## User should also have & own a home directory (for rstudio or linked volumes to work properly).
RUN apt-get update && apt-get install -y \
	libglpk-dev \
    libxml2-dev \
    librdf-query-client-perl

COPY . /study.wrangler
WORKDIR /study.wrangler

## Make a symlink in the rstudio homedir
RUN ln -s /study.wrangler /home/rstudio/study.wrangler

### CRAN
RUN R -e "install.packages('rdflib')"
RUN R -e "install.packages('/study.wrangler', repos=NULL , type='source')"

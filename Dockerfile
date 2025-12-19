FROM rocker/tidyverse:4

## system dependencies for building some R packages
RUN apt-get update && apt-get install -y \
	libglpk-dev \
	libxml2-dev

COPY . /study.wrangler
WORKDIR /study.wrangler

## Make a symlink in the rstudio homedir
RUN ln -s /study.wrangler /home/rstudio/study.wrangler

### local (installs github and BioConductor dependencies automatically)
RUN R -e "remotes::install_local('/study.wrangler', dependencies=TRUE)"

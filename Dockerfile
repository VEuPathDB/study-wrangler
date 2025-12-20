FROM rocker/tidyverse:4

## system dependencies for building some R packages
RUN apt-get update && apt-get install -y \
	libglpk-dev \
	libxml2-dev

WORKDIR /study.wrangler

## Make a symlink in the rstudio homedir
RUN ln -s /study.wrangler /home/rstudio/study.wrangler

## Copy only dependency metadata first (rarely changes)
## This allows Docker to cache the expensive package installation layer
COPY DESCRIPTION /study.wrangler/DESCRIPTION

## Install all dependencies (cached unless DESCRIPTION changes)
RUN R -e "remotes::install_deps('/study.wrangler', dependencies=TRUE)"

## NOW copy all the source code (this layer invalidates when code changes)
## But the dependency installation layer above will be cached!
COPY . /study.wrangler

## Install the local package (fast - dependencies already installed)
RUN R -e "install.packages('/study.wrangler', repos=NULL, type='source')"

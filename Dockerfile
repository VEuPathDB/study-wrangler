FROM rocker/tidyverse:4

## system dependencies for building some R packages
RUN apt-get update && apt-get install -y \
	libglpk-dev \
	libxml2-dev

COPY . /study.wrangler
WORKDIR /study.wrangler

## Make a symlink in the rstudio homedir
RUN ln -s /study.wrangler /home/rstudio/study.wrangler

### CRAN
RUN R -e "install.packages('skimr')"

### github

# plot.data dependencies not automatically installed:
RUN R -e "install.packages('BiocManager')"
RUN R -e "BiocManager::install('S4Vectors')"
RUN R -e "BiocManager::install('SummarizedExperiment')"
RUN R -e "BiocManager::install('DESeq2')"
RUN R -e "BiocManager::install('Maaslin2')"
RUN R -e "remotes::install_github('zdk123/SpiecEasi','v1.1.1', upgrade_dependencies=F)"
RUN R -e "remotes::install_github('VEuPathDB/veupathUtils', 'v2.7.0', upgrade_dependencies=F)"

# plot.data for binwidth function
RUN R -e "remotes::install_github('VEuPathDB/plot.data', 'v5.4.2', upgrade_dependencies=F)"


### local
RUN R -e "install.packages('/study.wrangler', repos=NULL , type='source')"

FROM rocker/tidyverse:4

COPY . /study.wrangler
WORKDIR /study.wrangler

## Make a symlink in the rstudio homedir
RUN ln -s /study.wrangler /home/rstudio/study.wrangler

### CRAN
RUN R -e "install.packages('skimr')"
RUN R -e "install.packages('/study.wrangler', repos=NULL , type='source')"

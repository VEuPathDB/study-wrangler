FROM rocker/tidyverse:4

## system dependencies for building some R packages
RUN apt-get update && apt-get install -y \
	libglpk-dev \
	libxml2-dev

# Install R packages via apt (pre-compiled, much faster than CRAN)
RUN apt-get update && apt-get install -y \
        r-cran-abind \
        r-cran-askpass \
        r-cran-backports \
        r-cran-base64enc \
        r-cran-bh \
        r-cran-biocmanager \
        r-cran-bit \
        r-cran-bit64 \
        r-cran-bitops \
        r-cran-blob \
        r-cran-broom \
        r-cran-bslib \
        r-cran-cachem \
        r-cran-callr \
        r-cran-cellranger \
        r-cran-cli \
        r-cran-clipr \
        r-cran-conflicted \
        r-cran-cpp11 \
        r-cran-crayon \
        r-cran-curl \
        r-cran-data.table \
        r-cran-dbi \
        r-cran-dbplyr \
        r-cran-digest \
        r-cran-dplyr \
        r-cran-dtplyr \
        r-cran-evaluate \
        r-cran-farver \
        r-cran-fastmap \
        r-cran-fontawesome \
        r-cran-forcats \
        r-cran-formatr \
        r-cran-fs \
        r-cran-futile.logger \
        r-cran-futile.options \
        r-cran-gargle \
        r-cran-generics \
        r-cran-ggplot2 \
        r-cran-glue \
        r-cran-googledrive \
        r-cran-googlesheets4 \
        r-cran-gtable \
        r-cran-haven \
        r-cran-highr \
        r-cran-hms \
        r-cran-htmltools \
        r-cran-httr \
        r-cran-ids \
        r-cran-igraph \
        r-cran-isoband \
        r-cran-jquerylib \
        r-cran-jsonlite \
        r-cran-knitr \
        r-cran-labeling \
        r-cran-lambda.r \
        r-cran-lifecycle \
        r-cran-locfit \
        r-cran-lubridate \
        r-cran-magrittr \
        r-cran-matrixstats \
        r-cran-memoise \
        r-cran-mime \
        r-cran-modelr \
        r-cran-openssl \
        r-cran-pillar \
        r-cran-pkgconfig \
        r-cran-prettyunits \
        r-cran-processx \
        r-cran-progress \
        r-cran-ps \
        r-cran-purrr \
        r-cran-r6 \
        r-cran-ragg \
        r-cran-rappdirs \
        r-cran-rcolorbrewer \
        r-cran-rcpp \
        r-cran-rcpparmadillo \
        r-cran-rcppeigen \
        r-cran-rcurl \
        r-cran-readr \
        r-cran-readxl \
        r-cran-rematch \
        r-cran-rematch2 \
        r-cran-repr \
        r-cran-reprex \
        r-cran-rlang \
        r-cran-rmarkdown \
        r-cran-rstudioapi \
        r-cran-rvest \
        r-cran-sass \
        r-cran-scales \
        r-cran-selectr \
        r-cran-skimr \
        r-cran-snow \
        r-cran-statmod \
        r-cran-stringi \
        r-cran-stringr \
        r-cran-sys \
        r-cran-systemfonts \
        r-cran-textshaping \
        r-cran-tibble \
        r-cran-tidyr \
        r-cran-tidyselect \
        r-cran-tidyverse \
        r-cran-timechange \
        r-cran-tinytex \
        r-cran-tzdb \
        r-cran-utf8 \
        r-cran-uuid \
        r-cran-vctrs \
        r-cran-viridislite \
        r-cran-vroom \
        r-cran-withr \
        r-cran-xfun \
        r-cran-xml2 \
        r-cran-yaml \
    && apt-get clean

## From now on, don't upgrade already-installed R packages (if they are sufficient)
ARG R_REMOTES_UPGRADE=never

# Raise the download timeout (R default is 60s, too short for slow
# Bioconductor/CRAN mirrors during package downloads).
RUN echo 'options(timeout = 600)' >> "$(R RHOME)/etc/Rprofile.site"

# Install remaining CRAN dependencies not available via apt, plus remotes
# (apt version of remotes is too old to handle the 'huge=url' remote type)
RUN R -e "install.packages(c('remotes', 'S7'))"

WORKDIR /study.wrangler

## Make a symlink in the rstudio homedir
RUN ln -s /study.wrangler /home/rstudio/study.wrangler

## Copy only dependency metadata first (rarely changes)
## This allows Docker to cache the expensive package installation layer
COPY DESCRIPTION /study.wrangler/DESCRIPTION

## Install all dependencies (cached unless DESCRIPTION changes)
## install_deps() exits 0 even when downloads fail, so retry a few times
## (failures are usually transient download timeouts; retries only re-fetch
## the still-missing packages). The final source install below is the gate
## that actually verifies the full dependency tree is present.
RUN R -e "for (i in 1:3) try(remotes::install_deps('/study.wrangler', dependencies=TRUE))"

## NOW copy all the source code (this layer invalidates when code changes)
## But the dependency installation layer above will be cached!
COPY . /study.wrangler

## Install the local package (fast - dependencies already installed).
## install.packages() exits 0 even when the install fails (e.g. a dependency
## is missing), so verify the package actually loads and fail the build
## otherwise — this exercises the whole dependency chain.
RUN R -e "install.packages('/study.wrangler', repos=NULL, type='source'); \
  if (!requireNamespace('study.wrangler', quietly=TRUE)) quit(status=1)"

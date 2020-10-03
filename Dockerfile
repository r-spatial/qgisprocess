FROM rocker/r-ubuntu:20.04

# install development version of QGIS
RUN wget -qO - https://qgis.org/downloads/qgis-2020.gpg.key | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/qgis-archive.gpg --import || true
RUN chmod a+r /etc/apt/trusted.gpg.d/qgis-archive.gpg && \
    add-apt-repository "deb https://qgis.org/ubuntu-nightly `lsb_release -c -s` main" && \
    apt-get update && \
    apt-get install -y qgis qgis-plugin-grass

# install R development packages
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev libfontconfig1-dev libgit2-dev
RUN Rscript -e "install.packages('devtools', dependencies = TRUE)"

# install qgisprocess development packages
# (a few at a time for better cacheing of the image)
RUN Rscript -e "remotes::install_cran(c('processx', 'stringr', 'withr', 'glue', 'tibble'))"

RUN apt-get update && apt-get install -y libgdal-dev libproj-dev libgeos-dev libudunits2-dev
RUN Rscript -e "remotes::install_cran(c('sf', 'sp', 'wk', 's2', 'raster', 'rgdal', 'stars', 'terra'))"

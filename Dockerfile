FROM rocker/r-ubuntu:20.04

RUN wget -qO - https://qgis.org/downloads/qgis-2020.gpg.key | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/qgis-archive.gpg --import && \
    chmod a+r /etc/apt/trusted.gpg.d/qgis-archive.gpg && \
    add-apt-repository "deb https://qgis.org/ubuntu-nightly `lsb_release -c -s` main" && \
    apt-get update && \
    apt-get install qgis qgis-plugin-grass

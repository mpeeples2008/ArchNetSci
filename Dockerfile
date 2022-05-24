# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.2.0

# required
MAINTAINER Matthew Peeples <Matthew.Peeples@asu.edu>

COPY . /ArchNetSci

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  # build this compendium package
  && R -e "devtools::install('/ArchNetSci', dep=TRUE)" \

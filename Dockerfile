# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.2.0

# required
MAINTAINER Matthew Peeples <Matthew.Peeeples@asu.edu>

COPY . /ArchNetSci

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # build this compendium package
  && R -e "devtools::install('/ArchNetSci')" \
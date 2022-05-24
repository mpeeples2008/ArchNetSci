# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.2.0

# required
MAINTAINER Matthew Peeples <Matthew.Peeples@asu.edu>

COPY . /ArchNetSci

# go into the repo directory
RUN apt-get update \ 
  && apt-get install -y \
    pandoc \
    texlive \
    texlive-latex-extra \
    texinfo \
    imagemagick \
  && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages("bookdown")
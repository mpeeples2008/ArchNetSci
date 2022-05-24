# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.2.0

# required
MAINTAINER Your Name <your_email@somewhere.com>

COPY . /ArchNetSci

# go into the repo directory
RUN . 


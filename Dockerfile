# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.2.0

# required
MAINTAINER Matthew Peeples <Matthew.Peeeples@asu.edu>

COPY . /ArchNetSci
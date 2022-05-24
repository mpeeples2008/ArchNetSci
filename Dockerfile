# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.2.0
# required
MAINTAINER Matthew Peeples <Matthew.Peeples@asu.edu>
COPY . /ArchNetSci
# go into the repo directory
RUN . su rstudio && \
  cd /home/rstudio && \
  wget https://github.com/mpeeples2008/ArchNetSci/archive/main.zip && \
  unzip main.zip && \
  mv ArchNetSci-main /home/rstudio/ArchNetSci && \
  cd ArchNetSci && \
RUN chown -Rv rstudio /home/rstudio/ArchNetSci

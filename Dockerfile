# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.2.0
# required
MAINTAINER Matthew Peeples <Matthew.Peeples@asu.edu>
# Copy repo into ${HOME}, make user own $HOME
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

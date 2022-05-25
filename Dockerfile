FROM rocker/binder:4.2

## Declares build arguments
ARG NB_USER
ARG NB_UID

COPY --chown=${NB_USER} . ${HOME}

ENV DEBIAN_FRONTEND=noninteractive
USER root
RUN echo "Checking for 'apt.txt'..." \
        ; if test -f "apt.txt" ; then \
        apt-get update --fix-missing > /dev/null\
        && xargs -a apt.txt apt-get install --yes \
        && apt-get clean > /dev/null \
        && rm -rf /var/lib/apt/lists/* \
        ; fi

## Install R packages ----
ENV RENV_VERSION 0.15.4
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
 && R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')" \
 && sudo -u rstudio R -e "renv::restore()"

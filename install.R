install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))
remotes::install_github('rstudio/renv@${RENV_VERSION}')
renv::restore()

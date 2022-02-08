# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.0.3

# required
MAINTAINER Ben Marwick <bmarwick@uw.edu>

WORKDIR /archyconfgender
COPY . /archyconfgender


# go into the repo directory
RUN . /etc/environment \
  R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
  && R -e "remotes::install_github('rstudio/renv')" \
  # install pkgs we need
  && R -e "renv::restore()" \
  # render the manuscript into a docx
  && R -e "rmarkdown::render('/archyconfgender/analysis/paper/paper.Rmd')"

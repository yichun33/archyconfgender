FROM rocker/binder:4.1.2
LABEL maintainer='Ben Marwick'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

# using renv to install the pkgs our project uses
RUN R -e "renv::restore(repos = c(CRAN='https://packagemanager.rstudio.com/all/__linux__/bionic/298'))"

RUN rm DESCRIPTION.1; exit 0

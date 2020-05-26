FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get -y install \
        libxml2-dev \
        texinfo \
        texlive \
        texlive-fonts-extra

COPY docker/bin /usr/local/bin/
RUN sed  -i'' '/mran.microsoft.com/d' /usr/local/lib/R/etc/Rprofile.site

RUN install_packages --repo=https://mrc-ide.github.io/drat \
    jsonlite \
    knitr \
    mockery \
    remotes \
    rmarkdown \
    testthat

RUN install_remote \
    mrc-ide/eppasm \
    mrc-ide/rrq \
    mrc-ide/hintr \
    reside-ic/traduire \
    reside-ic/pkgapi

COPY . /src
RUN R CMD INSTALL /src

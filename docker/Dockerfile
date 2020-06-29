FROM mrcide/naomi-base:latest

RUN apt-get update && apt-get -y install --no-install-recommends \
        libhiredis-dev \
        libjq-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

RUN install_packages --repo=https://mrc-ide.github.io/drat \
        docopt \
        geojsonio \
        glue \
        jsonlite \
        methods \
        plumber \
        ps \
        remotes \
        rlang

COPY docker/bin /usr/local/bin/

RUN install_remote \
        mrc-ide/heartbeatr \
        mrc-ide/rrq \
        reside-ic/traduire

COPY . /src
RUN R CMD INSTALL /src

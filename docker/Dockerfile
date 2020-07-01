FROM mrcide/naomi-base:latest

COPY docker/bin /usr/local/bin/

RUN install_remote \
        mrc-ide/naomi

COPY . /src
RUN cd /src/hintr && ./scripts/build_test_data
RUN R CMD INSTALL /src/hintr
RUN R CMD INSTALL /src

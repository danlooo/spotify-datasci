FROM rocker/verse:4.2.1

WORKDIR /build

COPY install-apt.sh ./
RUN sh install-apt.sh

COPY install.R ./
RUN Rscript install.R

ENV DISABLE_AUTH=true
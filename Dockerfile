FROM r-base
MAINTAINER Arnau Siches <asiches@gmail.com>

ENV TERM=xterm-256color

RUN apt-get update -qyy \
  && apt-get install -qyy \
    libgit2-dev \
    libxml2-dev \
    libcurl4-gnutls-dev \
  && rm -rf /var/lib/apt/lists/*

COPY dependencies.R dependencies.R
RUN Rscript dependencies.R

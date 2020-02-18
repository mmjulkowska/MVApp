FROM rocker/shiny:3.4.4

RUN apt-get update && \
    apt-get install vim libcurl4-openssl-dev libssl-dev \
                    libgmp-dev libmpfr-dev libxml2-dev \
                    libv8-3.14-dev -y --no-install-recommends && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN chown -R shiny:shiny /var/lib/shiny-server/bookmarks/shiny/

## Clean Up
RUN rm -rf /srv/shiny-server/*

## Make edits to global.R
COPY global.R /tmp
RUN sed -n '/# Load all the libraries/,$p' /tmp/global.R > /srv/shiny-server/global.R

## ADD files from repo to path 
COPY www/bullshit_out.jpg server.R ui.R /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server/

## Add config files
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /usr/bin/shiny-server.sh

#Install R libraries
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN R -e "install.packages($(grep 'list.of.packages <-' /tmp/global.R | cut -f 2 -d '-'))"

EXPOSE 80

CMD /usr/bin/shiny-server.sh

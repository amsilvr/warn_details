#FROM r-base:3.4.0
FROM rocker/shiny

RUN apt update -y \
	&& apt install -y python2.7 \
	cmake \
	gcc \
	g++ \
	git \
	libjpgalleg4-dev \
	libcurl4-openssl-dev \
	libssl-dev \
	libxml2-dev \
	libgdal-dev \
	libudunits2-dev \
	libpq-dev \
	gfortran

ADD . /srv/shiny-server
WORKDIR /srv/shiny-server

RUN R -e "install.packages('packrat')"
RUN R -e "packrat::restore()"


RUN R -e "install.packages(c('shiny', 'leaflet', 'DT', 'googlesheets', 'tidyverse', 'lubridate', 'sf', 'packrat', 'htmltab'), repos='https://cran.rstudio.com/')"

ADD start-shiny-server.sh /usr/bin/start-shiny-server.sh
ADD shiny-server.conf /etc/shiny-server

ENTRYPOINT ["/usr/bin/start-shiny-server.sh"]

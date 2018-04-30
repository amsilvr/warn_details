#!/bin/sh

#useradd -r -m shiny
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server
chown -R shiny.shiny /srv/shiny-server/
export SHINY_LOG_LEVEL=DEBUG
exec shiny-server >> /var/log/shiny-server.log 2>&1

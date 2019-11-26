#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny:shiny /var/log/shiny-server

exec shiny-server | tee -a /var/log/shiny-server.log

# Script for plotting and analysing the port-level species landings and price
# data from the CALFISH database:
# https://datadryad.org/stash/landing/show?id=doi%3A10.25349%2FD9M907

# Also accessed through wcfish package:
# https://github.com/cfree14/wcfish/
library(wcfish)
library(stringr)
library(dplyr)

# Read in annual port-level data
port_original <- read.csv("Data/CALFISH_port.csv")
# Select columns of interest
port <- port_original[, c("port", "type", "year", "comm_name", "value_usd",
                          "landings_lb")]

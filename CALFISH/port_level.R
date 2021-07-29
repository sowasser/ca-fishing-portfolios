# Script for plotting and analysing the port-level species landings and price
# data from the CALFISH database:
# https://datadryad.org/stash/landing/show?id=doi%3A10.25349%2FD9M907

calfish_original <- read.csv("Data/CALFISH_port.csv")

# Select columns of interest
calfish <- calfish_original[, c("port", "type", "year", "comm_name", "value_usd", "landings_lb")]

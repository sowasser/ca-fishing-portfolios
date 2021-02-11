# Workflow for each species in the the California landings & fishery
# participation data for 1992-2014, downloaded from the California Natural 
# Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014

library(dplyr)
library(ggplot2)
library(viridis)

port_landings <- read.csv("Data/port_landings_92-14.csv")

species <- levels(factor(port_landings$fishery))  # list of species

# Function for fishery timeseries by port -------------------------------------
species_timeseries <- function(species) {
  df <- port_landings %>% filter(fishery == species)
  df <- df %>% drop_na()
  
  plt <- ggplot(df, aes(x = year, y = ex.vessel_revenue, color = gear)) +
    theme_bw() +
    scale_color_viridis(discrete=TRUE) + #color of points from viridis
    geom_line(size=1.5) +  
    ggtitle(species) +
    ylab("revenue") + xlab(" ") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    facet_wrap(~port, ncol = 5)
  
  ggsave(filename=paste("Figures/Species revenue/", species, "_revenue.pdf", sep=""), 
         plot=plt, width=600, height=500, units="mm", dpi=300)
}


# Function call for each species ----------------------------------------------
species_timeseries("SALMON")

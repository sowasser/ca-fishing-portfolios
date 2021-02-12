# Workflow for each species in the the California landings & fishery
# participation data for 1992-2014, downloaded from the California Natural 
# Resources Agency:
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014

library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)
library(corrplot)

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
for (s in species) {
  species_timeseries(s)
}

# Timeseries of revenue for each species across all ports ---------------------
fish_sums <- group_by(port_landings, year, port, fishery) %>% 
  summarize(revenue = sum(ex.vessel_revenue))
fish_sums <- fish_sums %>% drop_na()
fish_sums2 <-  group_by(fish_sums, year, fishery) %>% 
  summarize(revenue = sum(revenue))

fish_revenue_sum <- ggplot(fish_sums2, aes(x = year, y = revenue)) +
  theme_bw() +
  geom_line(size=1.5) +  
  ylab("revenue") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~fishery, ncol = 6)

ggsave(filename=paste("Figures/Species revenue/allfisheries_revenue.pdf", sep=""), 
       plot=fish_revenue_sum, width=600, height=500, units="mm", dpi=300)

# Correlation matrix for all fisheries by year & port -------------------------
fish_sums3 <- dcast(fish_sums2, year ~ fishery)

fish_cor <- cor(fish_sums3[2:37])
corrplot(fish_cor, type="upper", tl.col = "black")

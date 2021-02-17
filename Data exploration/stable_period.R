# Script for initial analysis and summary statistics of the "stable period"
# from 2009-2014

library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)
library(corrplot)

port_landings <- read.csv("Data/port_landings_updated.csv")

pl_stable <- port_landings %>% filter(between(year, 2009, 2014))

fish_sums <- group_by(pl_stable, year, port, fishery) %>% 
  summarize(revenue = sum(ex.vessel_revenue))
fish_sums <- fish_sums %>% drop_na()
fish_sums2 <-  group_by(fish_sums, year, fishery) %>% 
  summarize(revenue = sum(revenue))

fish_revenue_sum <- ggplot(fish_sums2, aes(x = year, y = revenue)) +
  theme_bw() +
  geom_line(size=1.5) +  
  ylab("revenue") + xlab("stable period") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~fishery, ncol = 6)

ggsave(filename=paste("Figures/Stable period/allfisheries_revenue_stable.pdf", sep=""), 
       plot=fish_revenue_sum, width=600, height=500, units="mm", dpi=300)

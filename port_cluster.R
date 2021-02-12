# Script for exploring clusters in California ports based on the fishery
# species, number of fisheries, and revenue. Data cover 1992-2014 and were
# downlaoded from: 
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014
# Specifically, the "Landing Receipt Records: Landings Data By Port 1992-2014"

library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)

port_landings <- read.csv("Data/port_landings_92-14.csv")

# Find sum of revenue per fish species per port per year ----------------------
fish_sums <- group_by(port_landings, year, port, fishery) %>% 
  summarize(revenue = sum(ex.vessel_revenue))
fish_sums <- fish_sums %>% drop_na()

total_revenue <- group_by(fish_sums, year, port) %>%
  summarize(revenue = sum(revenue))

mean_revenue <- group_by(fish_sums, year, port) %>%
  summarize(revenue = mean(revenue))

fishery_count <- count(fish_sums, year, port)

rev_count <- cbind(total_revenue, fishery_count$n)
colnames(rev_count) <- c("year", "port", "revenue", "count")

# Scatter plot of ports by total revenue & number of fisheries 
rev_count$year <- as.factor(rev_count$year)
rev_count$port <- as.factor(rev_count$port)

revenue_count <- ggplot(rev_count, aes(x = count, y = revenue, color = port)) + 
  theme_bw() +
  geom_point(size = 3.5) + 
  xlab("number of fisheries") + 
  ylab("total revenue") + 
  theme(legend.title=element_blank()) +  # no legend title
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year, ncol = 5)

ggsave(filename="Figures/revenue_count.pdf", plot=revenue_count,
       width=600, height=500, units="mm", dpi=300)

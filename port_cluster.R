# Script for exploring clusters in California ports based on the fishery
# species, number of fisheries, and revenue. Data cover 1992-2014 and were
# downlaoded from: 
# https://data.cnra.ca.gov/dataset/human-uses-and-socioeconomic-dimensions-ca-north-coast-mpa-baseline-study-1992-2014
# Specifically, the "Landing Receipt Records: Landings Data By Port 1992-2014"

library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(reshape2)
library(cooccur)

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


# Which fish are fished together - co-occurrence of fishery by port & year -----
# Tutorial here - https://medium.com/analytics-vidhya/how-to-create-co-occurrence-networks-with-the-r-packages-cooccur-and-visnetwork-f6e1ceb1c523
# Subset original port_landings dataframe
pl_occurrence <- subset.data.frame(port_landings, 
                                   select = c("year", "port", "fishery", 
                                              "ex.vessel_revenue"))
# Combine multiple gear types together
pl_occurrence <- group_by(pl_occurrence, year, port, fishery) %>%
  summarize(revenue = sum(ex.vessel_revenue))

# Select data for a specific year
occur_2014 <- pl_occurrence %>% filter(year == 2014)
# Comvert to condensed shape
o2014 <- dcast(occur_2014, fishery ~ port)

# Convert revenue to ordinal data (1 for some revenue, 0 for NAs)
o2014[is.na(o2014)] <- 0  # replace NAs with 0
# Replace any value over 0 with 1
o2014 <- o2014 %>% mutate_if(is.numeric, ~1 * (. > 0))

# Set up data for cooccur - make fisheries the row names
co2014 <- o2014
row.names(co2014) <- co2014[, 1]
co2014 <- co2014[, -1]

# Run co-occurrence function & print output
co <- print(cooccur(co2014, spp_names = TRUE))


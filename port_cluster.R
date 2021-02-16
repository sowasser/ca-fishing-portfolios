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

port_landings <- read.csv("Data/port_landings_updated.csv")

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


# Which fish are fished together - co-occurrence of fishery by port  ----------
# Tutorial here - https://medium.com/analytics-vidhya/how-to-create-co-occurrence-networks-with-the-r-packages-cooccur-and-visnetwork-f6e1ceb1c523
# Subset original port_landings dataframe
pl_occurrence <- subset.data.frame(port_landings, 
                                   select = c("year", "port", "fishery", 
                                              "ex.vessel_revenue"))
# Combine multiple gear types together
pl_occurrence <- group_by(pl_occurrence, year, port, fishery) %>%
  summarize(revenue = sum(ex.vessel_revenue))

# Co-occurrence for all years together
occur <- dcast(pl_occurrence, fishery ~ port)  # re-arrange to wide format
occur <- occur %>% mutate_if(is.numeric, ~1 * (. > 0))  # replace all values > 0 with 1
row.names(occur) <- occur[, 1]  # make species name (1st column) the row names
occur <- occur[, -1]  # remove species name column

co <- cooccur(occur, spp_names = TRUE)  # run co-occurrence 
co_results <- co$results  # isolate results from cooccurr object
co_results <- as.data.frame(co_results)

# Subset of co-occurrence output where the probability of co-occurrence is at a
# frequency greater than the observed frequency
prob_occur <- co_results %>% filter(p_gt >= 0.95)


# Co-occurrence for each year -------------------------------------------------
# Function for species co-occurrence for each year in series
years <- 1992:2014 

co_occur <- function(yr) {
  occur <- pl_occurrence %>% filter(year == yr)  # select year
  
  occur <- dcast(occur, fishery ~ port)  # re-arrange to wide format
  occur[is.na(occur)] <- 0  # replace all NAs with 0 (no occurrence)
  occur <- occur %>% mutate_if(is.numeric, ~1 * (. > 0))  # replace all values > 0 with 1
  row.names(occur) <- occur[, 1]  # make species name (1st column) the row names
  occur <- occur[, -1]  # remove species name column
  
  # Run co-occurrence with the species name listed in output
  co <- cooccur(occur, spp_names = TRUE)
  co_results <- co$results  # isolate results from cooccurr object
  co_results <- as.data.frame(co_results)
  co2 <- cbind(rep(yr, length(co_results$sp1)), co_results) # combine with year
  colnames(co2)[1] <- "year"  # rename year column
  return(co2)  
}

# Call function for all years & merge into one dataframe
co_years <- list()  # empty list

for (y in years) {
  data <- co_occur(y)
  co_all[[y]] <- data
}

co_years <- bind_cols(co_years)

# Subset of co-occurrence output where the probability of co-occurrence is at a
# frequency greater than the observed frequency
prob_occur_years <- co_years %>% filter(p_gt >= 0.95)


# Script for examining response in monthly landings around the salmon closures
# in 2008 & 2009, using the CDFW dataset downloaded from here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

library(stringr)
library(dplyr)
library(reshape2)
library(PMCMRplus)
library(vegan)
library(iNEXT)
library(ggplot2)
library(viridis)

# Read in cleaned data with only the fisheries of interest & new year ranges
fishyears <- read.csv("Data/DFW areas/fisheries_year_soi.csv")
fishyears$month <- factor(fishyears$month, levels = c("Nov", "Dec", "Jan", 
                                                      "Feb", "Mar", "Apr", 
                                                      "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct"))

# Find sum across all areas - represents all of California
all_ca <- fishyears %>%
  group_by(species, year, month) %>%
  summarize(landings = sum(landings, na.rm = TRUE))

month_order <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                 "Aug", "Sep", "Oct")

# Separate closed & open years ------------------------------------------------
closed_years <- c("2007-2008", "2008-2009")
before_years <- c("1999-2000", "2000-2001", "2001-2002", "2002-2003", 
                  "2004-2005", "2005-2006", "2006-2007")
after_years <- c("2010-2011", "2011-2012", "2012-2013", "2013-2014", 
                 "2014-2015", "2015-2016", "2016-2017", "2017-2018", 
                 "2018-2019", "2019-2020")

# Find mean across all years and areas
closed_means <- all_ca %>% 
  filter(year %in% closed_years) %>%
  group_by(species, month) %>% 
  summarize(landings = mean(landings, na.rm = TRUE))

before_means <- all_ca %>%
  filter(year %in% before_years) %>%
  group_by(species, month) %>% 
  summarize(landings = mean(landings, na.rm = TRUE))

after_means <- all_ca %>%
  filter(year %in% after_years) %>%
  group_by(species, month) %>% 
  summarize(landings = mean(landings, na.rm = TRUE))

# Mean of total landings across all years, areas, and months
closed_all <- all_ca %>%
  filter(year %in% closed_years) %>%
  group_by(species) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
closed_all <- cbind(rep("closed", length(closed_all$species)), closed_all)
colnames(closed_all) <- c("period", "species", "landings")

before_all <- all_ca %>%
  filter(year %in% before_years) %>%
  group_by(species) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
before_all <- cbind(rep("before", length(before_all$species)), before_all)
colnames(before_all) <- c("period", "species", "landings")

after_all <- all_ca %>%
  filter(year %in% after_years) %>%
  group_by(species) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
after_all <- cbind(rep("after", length(after_all$species)), after_all)
colnames(after_all) <- c("period", "species", "landings")

# Standard deviation of total landings across all years, areas, and months
closed_sd <- all_ca %>%
  filter(year %in% closed_years) %>%
  group_by(species) %>%
  summarize(stdev = sd(landings, na.rm = TRUE))

before_sd <- all_ca %>%
  filter(year %in% before_years) %>%
  group_by(species) %>%
  summarize(stdev = sd(landings, na.rm = TRUE))

after_sd <- all_ca %>%
  filter(year %in% after_years) %>%
  group_by(species) %>%
  summarize(stdev = sd(landings, na.rm = TRUE))


# Diversity for species across all areas and years ----------------------------
# Shannon index is unitless, so hard to compare. See:
# https://www.researchgate.net/post/Comparing-Shannon-Index-H-values-between-two-communities 
shannon_closed <- diversity(closed_all$landings)
shannon_before <- diversity(before_all$landings)
shannon_after <- diversity(after_all$landings)


# Plot of species abundances in the different periods -------------------------
all_long <- rbind(before_all, closed_all, after_all)  # combine all

# Add standard deviations error bars & make sure periods are in correct order
sd_all <- rbind(before_sd, closed_sd, after_sd)
all_long <- cbind(all_long, sd_all$stdev)
colnames(all_long) <- c("period", "species","landings", "stdev")
all_long$period <- factor(all_long$period, levels = c("before", "closed", "after"))

all_species <- ggplot(all_long, aes(x = species, y = landings)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = landings-stdev, ymax = landings+stdev), 
                width = .2, position = position_dodge(.9)) +
  ylab("mean landings (lbs)") + xlab("fisheries of interest") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~period, ncol = 1)

ggsave(filename = "Monthly pdf data/Figures/Closures/salmon_all_species.pdf", 
       plot = all_species, width = 200, height = 320, units = "mm", dpi = 300)


# Compare fisheries of interest correlated with salmon across periods ---------
combine_periods <- function(sp_name) {
  # Combine the mean values for each period for a species
  before <- before_means %>% filter(species == sp_name)
  before <- cbind(rep("before", length(before$species)), before)
  colnames(before) <- c("period", "species", "month", "landings")
  
  closed <- closed_means %>% filter(species == sp_name)
  closed <- cbind(rep("closed", length(closed$species)), closed)
  colnames(closed) <- c("period", "species", "month", "landings")
  
  after <- after_means %>% filter(species == sp_name)
  after <- cbind(rep("after", length(after$species)), after)
  colnames(after) <- c("period", "species", "month", "landings")
  
  all <- rbind(before, closed, after)
  all$period <- factor(all$period, levels = c("before", "closed", "after"))
  return(all)
}


# Run for each species & run Kruskal-Wallis test between periods
# Herring Roe
roe <- combine_periods("Herring Roe")
kruskal.test(roe$landings ~ roe$period)  # p = 0.1802
kwAllPairsNemenyiTest(roe$landings ~ roe$period)

# Ocean Shrimp
shrimp <- combine_periods("Ocean Shrimp")
kruskal.test(shrimp$landings ~ shrimp$period)  # p = 0.2145
kwAllPairsNemenyiTest(shrimp$landings ~ shrimp$period)

# Red Sea Urchin*** 
urchin <- combine_periods("Red Sea Urchin")
kruskal.test(urchin$landings ~ urchin$period)  # p = 0.0001412***
kwAllPairsNemenyiTest(urchin$landings ~ urchin$period)

# Dungeness Crab 
crab <- combine_periods("Dungeness Crab")
kruskal.test(crab$landings ~ crab$period)  # p = 0.3162
kwAllPairsNemenyiTest(crab$landings ~ crab$period)

# Other Groundfish*** 
groundfish <- combine_periods("Other Groundfish")
kruskal.test(groundfish$landings ~ groundfish$period)  # p = 0.0001226***
kwAllPairsNemenyiTest(groundfish$landings ~ groundfish$period)

# Pacific Whiting* 
whiting <- combine_periods("Pacific Whiting")
kruskal.test(whiting$landings ~ whiting$period)  # p = 0.0295*
kwAllPairsNemenyiTest(whiting$landings ~ whiting$period)

# Dover Sole / Thornyhead / Sablefish* 
dsts <- combine_periods("Dover Sole_Thornyhead_Sablefish")
kruskal.test(dsts$landings ~ dsts$period)  # p = 0.02973*
kwAllPairsNemenyiTest(dsts$landings ~ dsts$period)

# Coastal Pelagics**** 
pelagics <- combine_periods("Pelagics")
kruskal.test(pelagics$landings ~ pelagics$period)  # p = 6.408e-05****
kwAllPairsNemenyiTest(pelagics$landings ~ pelagics$period)

# Market Squid 
squid <- combine_periods("Market Squid")
kruskal.test(squid$landings ~ squid$period)  # p = 0.8848
kwAllPairsNemenyiTest(squid$landings ~ squid$period)


# Plot species distributions across periods -----------------------------------
all_period_dist <- rbind(roe, shrimp, urchin, crab, groundfish, whiting, dsts, pelagics, squid)
dist_plots <- ggplot(all_period_dist, aes(x = month, y = landings, fill = period)) +
  theme_bw() +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis(discrete = TRUE) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~species, ncol = 3, scale = "free")

ggsave(filename = "Monthly pdf data/Figures/Closures/salmon_monthly_periods.pdf", 
       plot = dist_plots, width = 300, height = 200, units = "mm", dpi = 300)


# Timeseries of mean landings per year for each fishery of interest -----------
# Filter by specific fisheries of interest
soi <- c("Herring Roe", "Ocean Shrimp", "Red Sea Urchin", "Dungeness Crab",
         "Other Groundfish", "Pacific Whiting", 
         "Dover Sole_Thornyhead_Sablefish", "Pelagics", "Market Squid")

years_mean_soi <- fishyears %>% filter(species %in% soi) %>%
  group_by(species, year) %>%
  summarize(landings = mean(landings, na.rm = TRUE))

years_mean_soi$year <- as.factor(years_mean_soi$year)

# Create faceted plot with 
years_mean_plot <- ggplot(years_mean_soi, aes(x = year, y = landings, 
                                              fill = factor(ifelse(year=="2007-2008" | year=="2008-2009", 
                                                                   "closed", "open")))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(name = "salmon fishery", values = c("black", "grey50")) +
  ylab("mean landings (lbs)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~species, scale = "free", ncol = 3)

ggsave(filename = "Monthly pdf data/Figures/Closures/salmon_years_mean.pdf", 
       plot = years_mean_plot, width = 400, height = 300, units = "mm", dpi = 300)

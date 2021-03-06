# Script for stats on species parings (complimentary vs. substitutional) for 
# the DFW data, aggregated by area and month, for the species of interest.

# Information on Nemenyi post-hoc test here: 
# https://www.rdocumentation.org/packages/PMCMR/versions/4.3/topics/posthoc.kruskal.nemenyi.test

library(stringr)
library(tidyverse)
library(reshape2)
library(PMCMRplus)
library(kSamples)
library(ggplot2)
library(viridis)
library(scales)
library(ggsidekick)

fishyears <- read.csv("Data/DFW areas/fisheries_year_soi.csv")
fishyears$month <- factor(fishyears$month, levels = c("Nov", "Dec", "Jan", 
                                                      "Feb", "Mar", "Apr", 
                                                      "May", "Jun", "Jul",
                                                      "Aug", "Sep", "Oct"))

original_foi <- c("Market Squid", "Pelagics", "Dungeness Crab", "Red Sea Urchin",
                  "Ocean Shrimp", "Herring Roe", "Dover Sole_Thornyhead_Sablefish",
                  "Pacific Whiting", "Other Groundfish", "Salmon")
new_foi <- c("squid", "pelagics", "dungneness crab", "urchin", "shrimp", 
             "herring roe", "DSTS", "whiting", "groundfish", "salmon")


# Kruskal-Wallis test for the mean by year for top SOI ------------------------
all_ca <- fishyears %>%
  group_by(species, year, month) %>%
  summarize(landings = sum(landings, na.rm = TRUE))

kw_test_byyear <- function(sp_name) {
  df <- all_ca %>% filter(str_detect(species, sp_name))
  kw <- kruskal.test(df$landings ~ df$year)
  return(kw$p.value)
}

p_byyear <- sapply(original_foi, kw_test_species)
# print(p_byyear)


# Kruskal-Wallis test for the mean for each area across the stable period ----
area_means <- fishyears %>%
  group_by(species, area, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))

kw_test_byarea <- function(sp_name) {
  df <- area_means %>% filter(str_detect(species, sp_name))
  kw <- kruskal.test(df$landings ~ df$area)
  return(kw$p.value)
}

p_byarea <- sapply(original_foi, kw_test_byarea)
# print(p_byarea)


# Kruskal-Wallis test for the top fisheries of interest within each area ------
top_foi <- fishyears %>%
  filter(str_detect(species, "Market Squid|Pelagics|Dover Sole_Thornyhead_Sablefish|Pacific Whiting|Other Groundfish|Dungeness Crab|Red Sea Urchin|Ocean Shrimp|Herring Roe|Salmon"))

top_means <- top_foi %>% 
  group_by(species, area, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))
top_means$species <- factor(top_means$species, levels = original_foi, labels = new_foi)

areas <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", "Monterey", 
           "Morro Bay", "Santa Barbara", "Los Angeles", "San Diego")

kw_test_bysp <- function(area_name) {
  df <- top_means %>% filter(area == area_name)
  kw <- kruskal.test(df$landings ~ df$species)
  return(kw$p.value)
}

p_bysp <- sapply(areas, kw_test_bysp)
# print(p_bysp)


# Species distribution across months for all areas & years --------------------
allCA_foi_means <- all_ca %>%
  filter(str_detect(species, "Market Squid|Pelagics|Dover Sole_Thornyhead_Sablefish|Pacific Whiting|Other Groundfish|Dungeness Crab|Red Sea Urchin|Ocean Shrimp|Herring Roe|Salmon")) %>%
  group_by(species, month) %>%
  summarize(landings = mean(landings, na.rm = TRUE))

# Kruskal-Wallis for all means
allCA_foi_means$species <- as.factor(allCA_foi_means$species)
kruskal.test(allCA_foi_means$landings ~ allCA_foi_means$species)  # p = 2.403e-14****
nemenyi <- kwAllPairsNemenyiTest(allCA_foi_means$landings ~ allCA_foi_means$species)


# Substitutability index & matrix plot ----------------------------------------
# Calculate 0-1 index from Nemenyi p-values
sub_index <- melt(nemenyi$p.value)

# Replace NAs in rows with both species with 1
sub_index$value[as.character(sub_index$Var1) == as.character(sub_index$Var2)] <- 1

# Re-combine with DSTS & Salmon
sub_index <- rbind(data.frame(Var1 = "Dover Sole_Thornyhead_Sablefish",
                              Var2 = "Dover Sole_Thornyhead_Sablefish", 
                              value = 1.0000),
                   na.omit(sub_index),  # omit original NAs
                   data.frame(Var1 = "Salmon",
                              Var2 = "Salmon",
                              value = 1.0000))

sub_index$value[as.character(sub_index$Var1) == as.character(sub_index$Var2)] <- NA

# Re-cast into wide format with value for species=species & remove species column
sub_index <- dcast(sub_index, Var1 ~ Var2)[, -1]

# Map NAs in sub_index to values from a transposed version of sub_index
sub_index[] <- Map(function(x, y) {x[is.na(x)] <- y[is.na(x)]; x}, 
                   sub_index, data.table::transpose(sub_index))

# Re-name rows and columns
short <- c("DSTS", "dungeness crab", "herring roe", "market squid", 
           "ocean shrimp", "other groundfish", "Pacific whiting", "pelagics",
           "red sea urchin", "salmon")
row.names(sub_index) <- short
colnames(sub_index) <- short

# Plot by re-configuring ggcorrplot
sub_index_plot <- ggcorrplot::ggcorrplot(as.matrix(sub_index), hc.order = TRUE, lab = TRUE, type = "lower") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),  # only remove vertical grid lines
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()) +
  labs(x =" ", y = "", fill = "Substitutability Index") +
  scale_fill_viridis(limits = c(0, 1), direction = -1, alpha = 0.85) 
  # does it look better with the color scale reversed?? (direction = -1)

ggsave(filename = "Monthly pdf data/Figures/sub_index_plot.pdf", 
       plot = sub_index_plot, width = 195, height = 160, units = "mm", dpi = 300)


# Correlation between species -------------------------------------------------
allCA_means_cr <- dcast(allCA_foi_means, month ~ species)
colnames(allCA_means_cr) <- c("month", "DSTS", "dungeness crab", "herring roe", 
                              "market squid", "ocean shrimp", 
                              "other groundfish", "Pacific whiting", "pelagics",
                              "red sea urchin", "salmon")

species_cor <- round(cor(allCA_means_cr[, -1], method = "spearman"), 2)

species_cor_plot <- ggplot(melt(species_cor), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(height = 0.8, width = 0.8) +
  scale_fill_gradient2(low="blue", mid="white", high="red", limits = c(-1, 1)) +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme_minimal() +
  coord_equal() +
  labs(x =" ", y = "", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, margin = margin(-3,0,0,0)),
        axis.text.y = element_text(margin = margin(0, -3, 0, 0)),
        panel.grid.major = element_blank())

ggsave(filename = "Monthly pdf data/Figures/species_correlations.pdf", 
       plot = species_cor_plot, width = 195, height = 160, units = "mm", dpi = 300)


# Density plot of top species of interest -------------------------------------
den_allCA_foi <- allCA_foi_means
levels(den_allCA_foi$month) <- 1:12
den_allCA_foi$month <- as.numeric(den_allCA_foi$month)
den_allCA_foi$species <- factor(den_allCA_foi$species, levels = original_foi, labels = new_foi)

species_overlap <- ggplot(den_allCA_foi, aes(x = month, y = landings, fill = species)) +
  geom_area(position = "identity", alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7 ,8 ,9, 10, 11, 12), 
                     labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
                                "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
  xlab(" ") + ylab("mean landings") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_sleek()

ggsave(filename = "Monthly pdf data/Figures/species_overlap.pdf", 
       plot = species_overlap, width = 200, height = 130, units = "mm", dpi = 300)

# Script for plotting and analyzing the monthly species landings data from the
# CADFW website.

library(reshape2)
library(stringr)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggsidekick)

# Import monthly landings data by port complex from wcfish --------------------
monthly <- read.csv("Data/DFW areas/all_areas.csv")
monthly <- melt(monthly, id.vars = c("Species", "year", "area"))
colnames(monthly) <- c("species", "year", "port", "month", "landings")

# Update labels & specify order of months for better plots
monthly$month <- str_replace_all(monthly$month,
                                 c("January" = "Jan", "February" = "Feb", 
                                   "March" = "Mar", "April" = "Apr", 
                                   "June" = "Jun", "July" = "Jul", 
                                   "August" = "Aug", "September" = "Sep", 
                                   "October" = "Oct", "November" = "Nov", 
                                   "December" = "Dec"))
monthly$month <- factor(monthly$month, 
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Add taxonomic group generalization column ------------------------------------
# Create list of species names, if needed
all_species <- as.data.frame(levels(factor(monthly$species)))
write.csv(all_species, "~/Desktop/all_species.csv", row.names = FALSE)

# Create generalized categories
other_species <- "Algae|roe|Kelp|Terrapin|Turtle"
other_invert <- "Anemones|Bryozoan|Chiton|Invertebrate Unspecified|Invertebrates colonial|Jellyfish|Sea pansy|Spiders|Tunicates|Worms"
crustacean <- "Barnacle|Crab|Crayfish|Crustacean|Prawn|Shrimp"
mollusk <- "Abalone|Bat star|Clam|Echinoderm|Limpet|Mollusk unspecified|Mussel|Oyster|Sand dollar|Scallop|Sea cucumber|Sea hare|Sea slug|Sea stars|Sea urchin|Snail|Snails|Urchin|Whelk|Whelks"

# Multiple groupings for fish 
pelagic <- "Anchovy|Barracuda|Bonito|Cod|Herring Pacific|Herring round|Mackerel|Pomfret|Sardine|Saury|Silversides|Whiting"
flatfish <- "Flounder|Halibut|Sanddab|Sole|Tonguefish|Turbot"
migratory <- "Dolphin fish|Jacks|Jacksmelt|Kahawai|Marlin|Opah|Sailfish|Sierra|Swordfish|Spearfish|Sunfish|Tuna|Wahoo"

other_fish <- "Bass|Blackfish|Blacksmith|Bullhead|Butterfish|Cabrilla|Carp|Corbina|Corvina|Croaker|Eel|Escolar|Fish unspecified|Flyingfish|Goby|Grenadiers|Grouper|Grunion|Guitarfish|Hagfish|Halfmoon|Kelpfishes|Kelpfish giant|Lamprey|Lancelets|Lizardfish|Louvar|Midshipman|Mudsucker|Mullet|Needlefish|Oilfish|Opaleye|Pomfret|Prickleback|Queenfish|Ratfish|Ray|Sargo|Sculpin|Seabass|Seaperch|Senorita|Shad|Shark|Sheephead|Sierra Pacific|Skate|Smelt|Smelts|Splittail|Stickleback|Stingray||Sucker|Surfperch|Topsmelt|Trawled fish|Triggerfish|Trout|Whitefish|Wrasse|Yellowtail|Zebraperch"
rockfish <- "Rockfish|Scorpionfish"
roundfish <- "Cabezon|Cod|Greenling|Lingcod|Sablefish|Thornyhead|Tomcod"
salmon <- "Salmon"

all_fish <- paste(pelagic, flatfish, migratory, other_fish, rockfish, roundfish, salmon, sep = "|")

# Create dataframe with a column for each group & sum landings for the group
grouping <- function(group_string, group_name) {
  df <- monthly %>%
    filter(str_detect(species, group_string)) %>%
    mutate(group = group_name) %>%
    group_by(year, month, port, group) %>%
    summarize(landings_lb = sum(landings_lb))
}

monthly_fish <- bind_rows(grouping(pelagic, "coastal pelagic"),
                          grouping(flatfish, "flatfish"),
                          grouping(migratory, "highly migratory"),
                          grouping(other_fish, "other species"), 
                          grouping(rockfish, "rockfish"),
                          grouping(roundfish, "roundfish"),
                          grouping(salmon, "salmon"))

monthly_all <- bind_rows(grouping(other_species, "other"),
                         grouping(other_invert, "other invert"),
                         grouping(crustacean, "crustacean"),
                         grouping(mollusk, "mollusk"),
                         grouping(all_fish, "fish"))

# Plots of monthly landings by area -------------------------------------------
means_byarea <- monthly_all %>%
  group_by(month, port_complex, group) %>%
  summarize(landings = mean(landings_lb))

means_area_plot <- ggplot(means_byarea, aes(y = landings, x = month, fill = group)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port_complex, ncol = 3, scale = "free") +
  theme_sleek()

ggsave(filename="CALFISH/Figures/monthly_areas_all.pdf", means_area_plot,
       width=300, height=140, units="mm", dpi=300)


byarea_fish <- monthly_fish %>% 
  group_by(month, port_complex, group) %>%
  summarize(landings = mean(landings_lb))

area_fish_plot <- ggplot(byarea_fish, aes(y = landings, x = month, fill = group)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("mean landings (lbs)") + xlab(" ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port_complex, ncol = 3, scale = "free") +
  theme_sleek()

ggsave(filename="CALFISH/Figures/monthly_areas_fish.pdf", area_fish_plot,
       width=300, height=140, units="mm", dpi=300)

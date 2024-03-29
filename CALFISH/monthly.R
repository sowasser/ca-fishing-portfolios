# Script for plotting and analyzing the monthly species landings data from the
# CALFISH database:
# https://datadryad.org/stash/landing/show?id=doi%3A10.25349%2FD9M907

# Also accessed through wcfish package:
# https://github.com/cfree14/wcfish/
devtools::install_github("cfree14/wcfish", force=T)

library(wcfish)
library(stringr)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggsidekick)

# Import monthly landings data by port complex from wcfish --------------------
monthly <- swfsc[, c("year", "month", "port_complex", "comm_name_orig", 
                     "landings_lb")]

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
# all_species <- as.data.frame(levels(factor(monthly$comm_name_orig)))
# write.csv(all_species, "~/Desktop/all_species.csv", row.names = FALSE)

# Create generalized categories
other_species <- "Frogs|Herring Roe On Kelp|Turtles"
other_invert <- "Barnacle|Chiton|Cucumber, Sea|Echinod, Unspecified|Invertebrates, Colonial|Sea Stars|Urchin|Worms"
crustacean <- "Crab|Crayfish|Crustacean, Unspecified|Lobster|Prawn|Shrimp"
mollusk <- "Abalone|Clam|Limpet|Mollusk, Unspecified|Mussel|Octopus|Oyster|Scallop|Sea Hare|Sea Slug|Snail, Sea|Squid|Whelk"

# Multiple groupings for fish 
pelagic <- "Anchovy|Barracuda|Bonito|Cod|Herring,|Mackerel|Pomfret|Sardine|Whiting"
flatfish <- "Flounder|Halibut|Sanddab|Sole|Tonguefish|Turbot"
migratory <- "Dolphinfish|Jack Crevalle|Jacks|Kahawai|Marlin|Mola|Opah|Sailfish|Sierra|Swordfish|Tuna|Wahoo"
other_fish <- "Bass|Blackfish|Blacksmith|Bonefish|Butterfish|Cabrilla|Carp|Catfish|Corbina|Corvina|Croaker|Cusk-Eel|Eel|Escolar|Eulachon|Garibaldi|Goby|Grenadiers|Grouper|Grunion|Guitarfish|Hagfish|Halfmoon|Hardhead|Hitch|Kelpfishes|Killifish|Lizardfish|Louvar|Midshipman|Monkeyface Eel|Mudsucker|Mullet|Needlefish|Oilfish|Perch|Pikeminnow|Queenfish|Ratfish|Salema|Sculpin|Seabass, White|Senorita|Shad|Shark|Sheephead|Skate|Smelt|Snapper|Split-Tail|Stickleback|Stingray|Sturgeon|Sucker|Surfperch|Triggerfish|Unspecified Fish|Unspecified Trawled Fish|Whitefish|Wolf-Eel"
rockfish <- "Rockfish|Scorpionfish"
roundfish <- "Cabezon|Cod|Greenling, Kelp|Lingcod|Sablefish|Thornyhead|Tomcod"
salmon <- "Salmon"

all_fish <- paste(pelagic, flatfish, migratory, other_fish, rockfish, roundfish, salmon, sep = "|")

# Create dataframe with a column for each group & sum landings for the group
grouping <- function(group_string, group_name) {
  df <- monthly %>%
    filter(str_detect(comm_name_orig, group_string)) %>%
    mutate(group = group_name) %>%
    group_by(year, month, port_complex, group) %>%
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
  
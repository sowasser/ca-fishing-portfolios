# Script for plotting and analysing the port-level species landings and price
# data from the CALFISH database:
# https://datadryad.org/stash/landing/show?id=doi%3A10.25349%2FD9M907

# Also accessed through wcfish package:
# https://github.com/cfree14/wcfish/
library(wcfish)

# Read in annual port-level datasets
ports <- cdfw_ports[c("port_complex", "year", "comm_name", "value_usd")]

# Add taxonomic group generalization column ------------------------------------
# Create list of species names, if needed
all_species <- as.data.frame(levels(factor(ports$comm_name)))
write.csv(all_species, "~/Desktop/all_species_port.csv", row.names = FALSE)

# Create generalized categories
other_species <- "Frog|Giant kelp|Turtle|All other species|Kelp|algae|Miscellaneous|Terrapin"
other_invert <- "Anemone|Barnacle|star|chiton|invertebrate|urchin|Echinoderm|cucumber|Invertebrate|Jellyfish|flatworm|worm|Sand dollar|Bryozoan|Sea pansy|Sea spider|Sponge|Themiste|Tunicate"
crustacean <- "crab|shrimp|lobster|prawn|Crab|crayfish|Crustacean|Shrimp"
mollusk <- "Abalone|abalone|snail|clam|oyster|squid|whelk|limpet|Mollusk|Mussel|Octopus|Oyster|Scallop|Sea hare|Sea slug"

# Multiple groupings for fish 
pelagic <- "mackerel|barracuda|anchovy|sardine|Mackerel|bonito|hake|herring"
flatfish <- "flounder|sole|halibut|turbot|Flounder|Halibut|sanddab|Sanddab|Sole|Turbot"
migratory <- "tuna|dolphinfish|sunfish|Opah|Sailfish|spearfish|marlin|Swordfish|Tuna|Wahoo"
other_fish <- "shad|bass|surfperch|shark|ray|skate|croaker|Blacksmith|corbina|grunion|lizardfish|eel|needlefish|sheephead|carp|catfish|Croaker|Eel|Escolar|Flyingfish|kelpfish|Grenadier|Grouper|Hagfish|Halfmoon|Hardhead|Hitch|Jack|Jacksmelt|Kelpfish|Lancelet|mudsucker|Louvar|prickleback|smelt|whitefish|Oilfish|Opaleye|hagfish|pomfret|pompano|saury|sierra|Perch-like|midshipman|Pomfret|Queenfish|trout|Ray|wrasse|blackfish|Sargo|Senorita|Shark|corvina|guitarfish|Skate|Splittail|cabrilla|cusk-eel|ratfish|sculpin|Stingray|mullet|Sturgeon|Sucker|stickleback|Trawled fish|Triggerfish|seabass|Whitebait|eel|goby|amberjack|Zebraperch"
rockfish <- "rockfish|scorpionfish|Rockfish"
roundfish <- "Cabezon|greenling|Lingcod|thornyhead|Pacific cod|tomcod|Sablefish|Thornyhead"
salmon <- "salmon"

all_fish <- paste(pelagic, flatfish, migratory, other_fish, rockfish, roundfish, salmon, sep = "|")

# Create dataframe with a column for each group & sum landings for the group
grouping <- function(group_string, group_name) {
  df <- ports %>%
    filter(str_detect(comm_name, group_string)) %>%
    mutate(group = group_name) %>%
    group_by(year, port_complex, group) %>%
    summarize(value = sum(value_usd))
}

port_fish <- bind_rows(grouping(pelagic, "coastal pelagic"),
                       grouping(flatfish, "flatfish"),
                       grouping(migratory, "highly migratory"),
                       grouping(other_fish, "other species"), 
                       grouping(rockfish, "rockfish"),
                       grouping(roundfish, "roundfish"),
                       grouping(salmon, "salmon"))

port_all <- bind_rows(grouping(other_species, "other"),
                      grouping(other_invert, "other invert"),
                      grouping(crustacean, "crustacean"),
                      grouping(mollusk, "mollusk"),
                      grouping(all_fish, "fish"))


# Plot yearly value (usd) -----------------------------------------------------
yearly_value_all <- ggplot(port_all, aes(y = value, x = year, fill = group)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("total value (USD)") + xlab(" ") +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port_complex, ncol = 3, scale = "free_y") +
  theme_sleek()

ggsave(filename="CALFISH/Figures/yearly_value_all.pdf", yearly_value_all,
       width=300, height=220, units="mm", dpi=300)

yearly_value_fish <- ggplot(port_fish, aes(y = value, x = year, fill = group)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("total value (USD)") + xlab(" ") +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~port_complex, ncol = 3, scale = "free_y") +
  theme_sleek()

ggsave(filename="CALFISH/Figures/yearly_value_fish.pdf", yearly_value_fish,
       width=300, height=220, units="mm", dpi=300)

# Script for plotting and analysing the port-level species landings and price
# data from the CALFISH database:
# https://datadryad.org/stash/landing/show?id=doi%3A10.25349%2FD9M907

# Also accessed through wcfish package:
# https://github.com/cfree14/wcfish/
library(wcfish)

# Read in annual port-level data
port_original <- read.csv("Data/CALFISH_port.csv")
# Select columns of interest
port <- port_original[, c("port", "type", "year", "comm_name", "value_usd",
                          "landings_lb")]

# Import monthly landings data by port complex from wcfish --------------------
monthly_original <- swfsc
monthly <- monthly_original[, c("year", "month", "port_complex", 
                                "comm_name_orig", "landings_lb")]

# Taxonomic group generalization column ---------------------------------------
# Create list of species names
all_species <- as.data.frame(levels(factor(monthly$comm_name_orig)))
write.csv(all_species, "~/Desktop/all_species.csv", row.names = FALSE)

# Create generalized categories
other_species <- "Frogs|Herring Roe On Kelp|Turtles"
other_invertebrates <- "Barnacle|Chiton|Cucumber, Sea|Echinod, Unspecified|Invertebrates, Colonial|Sea Stars|Urchin|Worms"
crustacean <- "Crab|Crayfish|Crustacean, Unspecified|Lobster|Prawn|Shrimp"
mollusk <- "Abalone|Clam|Limpet|Mollusk, Unspecified|Mussel|Octopus|Oyster|Scallop|Sea Hare|Sea Slug|Snail, Sea|Squid|Whelk"

# Multiple groupings for fish 
pelagics <- "Anchovy|Barracuda|Bonito|Cod|Herring,|Mackerel|Pomfret|Sardine|Whiting"
flatfish <- "Flounder|Halibut|Sanddab|Sole|Tonguefish|Turbot"
migratory <- "Dolphinfish|Jack Crevalle|Jacks|Kahawai|Marlin|Mola|Opah|Sailfish|Sierra|Swordfish|Tuna|Wahoo"
other_fish <- "Bass|Blackfish|Blacksmith|Bonefish|Butterfish|Cabrilla|Carp|Catfish|Corbina|Corvina|Croaker|Cusk-Eel|Eel|Escolar|Eulachon|Garibaldi|Goby|Grenadiers|Grouper|Grunion|Guitarfish|Hagfish|Halfmoon|Hardhead|Hitch|Kelpfishes|Killifish|Lizardfish|Louvar|Midshipman|Monkeyface Eel|Mudsucker|Mullet|Needlefish|Oilfish|Perch|Pikeminnow|Queenfish|Ratfish|Salema|Sculpin|Seabass, White|Senorita|Shad|Shark|Sheephead|Skate|Smelt|Snapper|Split-Tail|Stickleback|Stingray|Sturgeon|Sucker|Surfperch|Triggerfish|Unspecified Fish|Unspecified Trawled Fish|Whitefish|Wolf-Eel"
rockfish <- "Rockfish|Scorpionfish"
roundfish <- "Cabezon|Cod|Greenling, Kelp|Lingcod|Sablefish|Thornyhead|Tomcod"
salmon <- "Salmon"

fish <- paste(pelagics, flatfish, migratory, other_fish, rockfish, roundfish, salmon, sep = "|")

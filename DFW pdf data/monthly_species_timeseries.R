# Analysis of the data from the nine areas within California, gathered from the
# California Department of Natural Resources, accessed here: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings

# In this script, graphs of monthly landings for each of the species, faceted 
# by year 

library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)


# Import datasets for all areas and for species of interest
all_areas <- read.csv("Data/DFW areas/all_areas.csv")
all_soi <- read.csv("Data/dfw_areas_all_soi.csv")
top_soi <- read.csv("Data/dfw_areas_top_soi.csv")

# Order of areas from North -> South
area_order <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", 
                "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles",
                "San Diego")

# Abbreviation of months for nice plots
months_abbrev <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                   "Sep", "Oct", "Nov", "Dec")


# Transform data into a useful form
soi_plots <- all_soi[, -14]  # create new df for plots & remove total landings
soi_plots$area <- factor(soi_plots$area, levels = area_order)
colnames(soi_plots) <- c("species", months_abbrev, "year", "area")
soi_plots <- soi_plots[, c(1, 15, 14, 12, 13, 2:11)]
soi_plots$year <- as.character(soi_plots$year)
soi_plots <- melt(soi_plots, id_vars = c("species", "area", "year"))
colnames(soi_plots) <- c("species", "area", "year", "month", "landings")


# Replace simple years with a year range --------------------------------------
df <- within(soi_plots, year[year == "2000" & month == "Nov" | 
                               year == "2000" & month == "Dec"] <- "2000-2001")
df <- within(df, year[year == "2000" & month == "Jan" | year == "2000" & month == "Feb" |
                        year == "2000" & month == "Mar" | year == "2000" & month == "Apr" | 
                        year == "2000" & month == "May" | year == "2000" & month == "Jun" | 
                        year == "2000" & month == "Jul" | year == "2000" & month == "Aug" |
                        year == "2000" & month == "Sep" | year == "2000" & month == "Oct"] 
             <- "1999-2000")

df <- within(df, year[year == "2001" & month == "Nov" | 
                        year == "2001" & month == "Dec"] <- "2001-2002")
df <- within(df, year[year == "2001" & month == "Jan" | year == "2001" & month == "Feb" |
                        year == "2001" & month == "Mar" | year == "2001" & month == "Apr" | 
                        year == "2001" & month == "May" | year == "2001" & month == "Jun" | 
                        year == "2001" & month == "Jul" | year == "2001" & month == "Aug" |
                        year == "2001" & month == "Sep" | year == "2001" & month == "Oct"] 
             <- "2000-2001")

df <- within(df, year[year == "2002" & month == "Nov" | 
                        year == "2002" & month == "Dec"] <- "2002-2003")
df <- within(df, year[year == "2002" & month == "Jan" | year == "2002" & month == "Feb" |
                        year == "2002" & month == "Mar" | year == "2002" & month == "Apr" | 
                        year == "2002" & month == "May" | year == "2002" & month == "Jun" | 
                        year == "2002" & month == "Jul" | year == "2002" & month == "Aug" |
                        year == "2002" & month == "Sep" | year == "2002" & month == "Oct"] 
             <- "2001-2002")

df <- within(df, year[year == "2003" & month == "Nov" | 
                        year == "2003" & month == "Dec"] <- "2003-2004")
df <- within(df, year[year == "2003" & month == "Jan" | year == "2003" & month == "Feb" |
                        year == "2003" & month == "Mar" | year == "2003" & month == "Apr" | 
                        year == "2003" & month == "May" | year == "2003" & month == "Jun" | 
                        year == "2003" & month == "Jul" | year == "2003" & month == "Aug" |
                        year == "2003" & month == "Sep" | year == "2003" & month == "Oct"] 
             <- "2002-2003")

df <- within(df, year[year == "2004" & month == "Nov" | 
                        year == "2004" & month == "Dec"] <- "2004-2005")
df <- within(df, year[year == "2004" & month == "Jan" | year == "2004" & month == "Feb" |
                        year == "2004" & month == "Mar" | year == "2004" & month == "Apr" | 
                        year == "2004" & month == "May" | year == "2004" & month == "Jun" | 
                        year == "2004" & month == "Jul" | year == "2004" & month == "Aug" |
                        year == "2004" & month == "Sep" | year == "2004" & month == "Oct"] 
             <- "2003-2004")

df <- within(df, year[year == "2005" & month == "Nov" | 
                        year == "2005" & month == "Dec"] <- "2005-2006")
df <- within(df, year[year == "2005" & month == "Jan" | year == "2005" & month == "Feb" |
                        year == "2005" & month == "Mar" | year == "2005" & month == "Apr" | 
                        year == "2005" & month == "May" | year == "2005" & month == "Jun" | 
                        year == "2005" & month == "Jul" | year == "2005" & month == "Aug" |
                        year == "2005" & month == "Sep" | year == "2005" & month == "Oct"] 
             <- "2004-2005")

df <- within(df, year[year == "2006" & month == "Nov" | 
                        year == "2006" & month == "Dec"] <- "2006-2007")
df <- within(df, year[year == "2006" & month == "Jan" | year == "2006" & month == "Feb" |
                        year == "2006" & month == "Mar" | year == "2006" & month == "Apr" | 
                        year == "2006" & month == "May" | year == "2006" & month == "Jun" | 
                        year == "2006" & month == "Jul" | year == "2006" & month == "Aug" |
                        year == "2006" & month == "Sep" | year == "2006" & month == "Oct"] 
             <- "2005-2006")

df <- within(df, year[year == "2007" & month == "Nov" | 
                        year == "2007" & month == "Dec"] <- "2007-2008")
df <- within(df, year[year == "2007" & month == "Jan" | year == "2007" & month == "Feb" |
                        year == "2007" & month == "Mar" | year == "2007" & month == "Apr" | 
                        year == "2007" & month == "May" | year == "2007" & month == "Jun" | 
                        year == "2007" & month == "Jul" | year == "2007" & month == "Aug" |
                        year == "2007" & month == "Sep" | year == "2007" & month == "Oct"] 
             <- "2006-2007")

df <- within(df, year[year == "2008" & month == "Nov" | 
                        year == "2008" & month == "Dec"] <- "2008-2009")
df <- within(df, year[year == "2008" & month == "Jan" | year == "2008" & month == "Feb" |
                        year == "2008" & month == "Mar" | year == "2008" & month == "Apr" | 
                        year == "2008" & month == "May" | year == "2008" & month == "Jun" | 
                        year == "2008" & month == "Jul" | year == "2008" & month == "Aug" |
                        year == "2008" & month == "Sep" | year == "2008" & month == "Oct"] 
             <- "2007-2008")

df <- within(df, year[year == "2009" & month == "Nov" | 
                        year == "2009" & month == "Dec"] <- "2009-2010")
df <- within(df, year[year == "2009" & month == "Jan" | year == "2009" & month == "Feb" |
                        year == "2009" & month == "Mar" | year == "2009" & month == "Apr" | 
                        year == "2009" & month == "May" | year == "2009" & month == "Jun" | 
                        year == "2009" & month == "Jul" | year == "2009" & month == "Aug" |
                        year == "2009" & month == "Sep" | year == "2009" & month == "Oct"] 
             <- "2008-2009")

df <- within(df, year[year == "2010" & month == "Nov" | 
                        year == "2010" & month == "Dec"] <- "2010-2011")
df <- within(df, year[year == "2010" & month == "Jan" | year == "2010" & month == "Feb" |
                        year == "2010" & month == "Mar" | year == "2010" & month == "Apr" | 
                        year == "2010" & month == "May" | year == "2010" & month == "Jun" | 
                        year == "2010" & month == "Jul" | year == "2010" & month == "Aug" |
                        year == "2010" & month == "Sep" | year == "2010" & month == "Oct"] 
             <- "2009-2010")

df <- within(df, year[year == "2011" & month == "Nov" | 
                        year == "2011" & month == "Dec"] <- "2011-2012")
df <- within(df, year[year == "2011" & month == "Jan" | year == "2011" & month == "Feb" |
                        year == "2011" & month == "Mar" | year == "2011" & month == "Apr" | 
                        year == "2011" & month == "May" | year == "2011" & month == "Jun" | 
                        year == "2011" & month == "Jul" | year == "2011" & month == "Aug" |
                        year == "2011" & month == "Sep" | year == "2011" & month == "Oct"] 
             <- "2010-2011")

df <- within(df, year[year == "2012" & month == "Nov" | 
                        year == "2012" & month == "Dec"] <- "2012-2013")
df <- within(df, year[year == "2012" & month == "Jan" | year == "2012" & month == "Feb" |
                        year == "2012" & month == "Mar" | year == "2012" & month == "Apr" | 
                        year == "2012" & month == "May" | year == "2012" & month == "Jun" | 
                        year == "2012" & month == "Jul" | year == "2012" & month == "Aug" |
                        year == "2012" & month == "Sep" | year == "2012" & month == "Oct"] 
             <- "2011-2012")

df <- within(df, year[year == "2013" & month == "Nov" | 
                        year == "2013" & month == "Dec"] <- "2013-2014")
df <- within(df, year[year == "2013" & month == "Jan" | year == "2013" & month == "Feb" |
                        year == "2013" & month == "Mar" | year == "2013" & month == "Apr" | 
                        year == "2013" & month == "May" | year == "2013" & month == "Jun" | 
                        year == "2013" & month == "Jul" | year == "2013" & month == "Aug" |
                        year == "2013" & month == "Sep" | year == "2013" & month == "Oct"] 
             <- "2012-2013")

df <- within(df, year[year == "2014" & month == "Nov" | 
                        year == "2014" & month == "Dec"] <- "2014-2015")
df <- within(df, year[year == "2014" & month == "Jan" | year == "2014" & month == "Feb" |
                        year == "2014" & month == "Mar" | year == "2014" & month == "Apr" | 
                        year == "2014" & month == "May" | year == "2014" & month == "Jun" | 
                        year == "2014" & month == "Jul" | year == "2014" & month == "Aug" |
                        year == "2014" & month == "Sep" | year == "2014" & month == "Oct"] 
             <- "2013-2014")

df <- within(df, year[year == "2015" & month == "Nov" | 
                        year == "2015" & month == "Dec"] <- "2015-2016")
df <- within(df, year[year == "2015" & month == "Jan" | year == "2015" & month == "Feb" |
                        year == "2015" & month == "Mar" | year == "2015" & month == "Apr" | 
                        year == "2015" & month == "May" | year == "2015" & month == "Jun" | 
                        year == "2015" & month == "Jul" | year == "2015" & month == "Aug" |
                        year == "2015" & month == "Sep" | year == "2015" & month == "Oct"] 
             <- "2014-2015")

df <- within(df, year[year == "2016" & month == "Nov" | 
                        year == "2016" & month == "Dec"] <- "2016-2017")
df <- within(df, year[year == "2016" & month == "Jan" | year == "2016" & month == "Feb" |
                        year == "2016" & month == "Mar" | year == "2016" & month == "Apr" | 
                        year == "2016" & month == "May" | year == "2016" & month == "Jun" | 
                        year == "2016" & month == "Jul" | year == "2016" & month == "Aug" |
                        year == "2016" & month == "Sep" | year == "2016" & month == "Oct"] 
             <- "2015-2016")

df <- within(df, year[year == "2017" & month == "Nov" | 
                        year == "2017" & month == "Dec"] <- "2017-2018")
df <- within(df, year[year == "2017" & month == "Jan" | year == "2017" & month == "Feb" |
                        year == "2017" & month == "Mar" | year == "2017" & month == "Apr" | 
                        year == "2017" & month == "May" | year == "2017" & month == "Jun" | 
                        year == "2017" & month == "Jul" | year == "2017" & month == "Aug" |
                        year == "2017" & month == "Sep" | year == "2017" & month == "Oct"] 
             <- "2016-2017")

df <- within(df, year[year == "2018" & month == "Nov" | 
                        year == "2018" & month == "Dec"] <- "2018-2019")
df <- within(df, year[year == "2018" & month == "Jan" | year == "2018" & month == "Feb" |
                        year == "2018" & month == "Mar" | year == "2018" & month == "Apr" | 
                        year == "2018" & month == "May" | year == "2018" & month == "Jun" | 
                        year == "2018" & month == "Jul" | year == "2018" & month == "Aug" |
                        year == "2018" & month == "Sep" | year == "2018" & month == "Oct"] 
             <- "2017-2018")

df <- within(df, year[year == "2019" & month == "Nov" | 
                        year == "2019" & month == "Dec"] <- "2019-2020")
df <- within(df, year[year == "2019" & month == "Jan" | year == "2019" & month == "Feb" |
                        year == "2019" & month == "Mar" | year == "2019" & month == "Apr" | 
                        year == "2019" & month == "May" | year == "2019" & month == "Jun" | 
                        year == "2019" & month == "Jul" | year == "2019" & month == "Aug" |
                        year == "2019" & month == "Sep" | year == "2019" & month == "Oct"] 
             <- "2018-2019")

# Remove rows that include years without any data
df <- df %>% filter(!year == "1999-2000" & !year == "2019-2020")

# Rename temporary dataframe to match previous dataframe
soi_plots <- df

# Create plots in a loop ------------------------------------------------------
soi_fisheries <- levels(factor(soi_plots$species))

species_timeseries <- function(fishery) {
  df <- soi_plots %>% filter(species == fishery)
  
  plt <- ggplot(df, aes(x = month, y = landings, fill = area)) +
    geom_bar(position = "stack", stat = "identity") +
    ylab("mean landings (lbs)") + xlab(" ") +
    ggtitle(fishery) + 
    scale_fill_viridis(discrete = TRUE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    facet_wrap(~year, ncol = 4, scale = "free")
  
  ggsave(filename=paste("DFW pdf data/Figures/Species timeseries/", fishery, "_revenue.pdf", 
                        sep=""), 
         plot=plt, width=400, height=250, units="mm", dpi=300)
}

# Function call for each species 
for (s in soi_fisheries) {
  species_timeseries(s)
}

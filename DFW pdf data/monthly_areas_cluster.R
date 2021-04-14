# Script for investigating how the monthly landings data (downloaded from the 
# California Department of Natural Resources: 
# https://wildlife.ca.gov/Fishing/Commercial/Landings) are clustered.

# Clustering tutorial: 
# https://towardsdatascience.com/clustering-datasets-having-both-numerical-and-categorical-variables-ed91cdca0677

library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2)

fishyear <- read.csv("Data/DFW areas/fisheries_year_soi.csv")

# Convert categorial variables to factors
fishyear$area <- as.factor(fishyear$area)
fishyear$year <- as.factor(fishyear$year)
fishyear$month <- as.factor(fishyear$month)

# Isolate squid and remove species column
squid <- fishyear %>% filter(str_detect(species, "Squid"))
squid <- squid[, -1]

# Create dissimilarity matrix using Gower metric
gower_df <- daisy(squid, metric = "gower")

# Determine ideal number of clusters
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette, pam_clusters$silinfo$avg.width)
}

plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

# Examine medoids
pam <- pam(gower_df, diss = TRUE, k = 6)
squid[pam$meoids, ]

# Conduct partitioning "around medoids"
pam_summary <- squid %>% 
  mutate(cluster = pam$clustering) %>% 
  group_by(cluster) %>% 
  do(cluster_summary = summary(.))

pam_summary$cluster_summary[[1]]

# Plot result
tsne_object <- Rtsne(gower_df, is_distance = TRUE)

tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam$clustering))

ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))


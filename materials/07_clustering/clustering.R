#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 7: Clustering ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Download latest {EGAnet} from GitHub
remotes::install_github("hfgolino/EGAnet")

# Load packages
library(ggplot2); library(factoextra); library(cluster)
library(NbClust); library(igraph); library(EGAnet)

# Set seed
set.seed(42)

#%%%%%%%%%%%%%%%%%%
## Data set up ----
#%%%%%%%%%%%%%%%%%%

# Load data
salary <- read.csv("../../data/university_salary/schools_that_pay.csv")

# Select available data
salary_voi <- salary[,-c(5, 8)]

# Regions
unique(salary_voi$region)

# Extract numeric values
salary_numeric <- salary_voi[,-c(1:2)]

# Scale numeric values
salary_numeric <- scale(salary_numeric)

#%%%%%%%%%%%%%%%%
## K-mediods ----
#%%%%%%%%%%%%%%%%

# Convert to distance
salary_distance <- daisy(salary_numeric, metric = "gower")

# Basic heatmap
gowers_heatmap <- EGAnet:::ggheatmap(salary_distance) +
  scale_fill_gradient(
    name = "Gower's Distance",  limits = c(0, 1),
    low = "lightgrey", high = "#A3C4BC"
  ) + theme(
    axis.text = element_blank(), axis.title = element_blank(),
    axis.ticks = element_blank(), legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ); gowers_heatmap

# Perform k-mediods based on number regions
theoretical_run <- pam(
  x = salary_distance, # supply distance
  k = 5, # number of clusters
  nstart = 25 # number of random starting values
)

# Median observations
salary_voi[theoretical_run$medoids,]

# Need to supply data back to object
theoretical_run$data <- salary_numeric

# Let's visualize
fviz_cluster(theoretical_run)

# Make copy of theoretical
actual <- theoretical_run

# Replace clusters
actual$clustering <- as.numeric(factor(salary_voi$region))

# What does the region clusters look like?
fviz_cluster(object = actual, data = salary_numeric)

# Is there strong similarity between region and empirical clusters?
## Adjusted Rand Index
compare(
  actual$clustering, theoretical_run$clustering,
  method = "adjusted.rand"
)

## Normalized Mutual Information
compare(
  actual$clustering, theoretical_run$clustering,
  method = "nmi"
)

## Raw proportion
sum(diag(table(
  actual$cluster, theoretical_run$clustering
))) / length(actual$cluster)

# Plot "elbow" method
fviz_nbclust(
  x = salary_numeric, # supply data
  FUNcluster = pam, # cluster function
  diss = salary_distance, # supply distance
  method = "wss", # within-cluster sum of squares
  k = 10,  # maximum number of clusters
  nstart = 25 # same as our k-mediods setup
)

# Plot silhouette method
fviz_nbclust(
  x = salary_numeric, # supply data
  FUNcluster = pam, # cluster function
  diss = salary_distance, # supply distance
  method = "silhouette", # silhouette
  k = 10,  # maximum number of clusters
  nstart = 25 # same as our k-mediods setup
)

# Perform k-mediods with silhouette
silhouette_run <- pam(
  x = salary_distance, # supply distance
  k = 2, # number of clusters
  nstart = 25 # number of random starting values
)

# Need to supply data back to object
silhouette_run$data <- salary_numeric

# Plot
fviz_cluster(silhouette_run, salary_numeric)

# Median observations
salary_voi[silhouette_run$medoids,]
# Higher versus lower outcome in terms of median salary

# Where does Vanderbilt come out?
silhouette_run$cluster[grepl("Vanderbilt", salary$school_name)]

#%%%%%%%%%%%%%%%%%%%
## Hierarchical ----
#%%%%%%%%%%%%%%%%%%%

# Perform agglomerative clustering based on number regions
hierarchical <- agnes(
  x = salary_distance, # supply distance
  method = "complete" # linkage method
)

# Agglomerative coefficient
hierarchical$ac

# Ensure layout of plots to be a single plot
# (plotting without this will plot weirdly)
layout(matrix(1))

# Plot dendrogram
plot(hierarchical, which = 2)
abline(h = 0.60, col = "red", lty = 2)
abline(h = 0.45, col = "blue", lty = 2)
abline(h = 0.40, col = "purple", lty = 2)

# A cut needs to be made to actually get clusters
theoretical_clusters <- cutree(hierarchical, k = 5)

# Is there strong similarity between region and empirical clusters?
## Adjusted Rand Index
compare(
  actual$clustering, theoretical_clusters,
  method = "adjusted.rand"
)

## Normalized Mutual Information
compare(
  actual$clustering, theoretical_clusters,
  method = "nmi"
)

## Raw proportion
sum(diag(table(
  actual$cluster, theoretical_clusters
))) / length(actual$cluster)

# Plot "elbow" method
fviz_nbclust(
  x = salary_numeric, # supply data
  FUNcluster = hcut, # cluster function
  hc_func = "agnes", # ensures same approach
  hc_method = "complete", # ensures same approach
  diss = salary_distance, # supply distance
  method = "wss" # within-cluster sum of squares
)

# Plot silhouette method
fviz_nbclust(
  x = salary_numeric, # supply data
  FUNcluster = hcut, # cluster function
  hc_func = "agnes", # ensures same approach
  hc_method = "complete", # ensures same approach
  diss = salary_distance, # supply distance
  method = "silhouette" # within-cluster sum of squares
)

# Get agglomerative clusters based on silhouette
silhouette_clusters <- cutree(hierarchical, k = 2)

# Plot with clusters
pseudo_info <- list(
  clusters = silhouette_clusters,
  clusterTree = hierarchical,
  JSD = as.matrix(salary_distance)
)

# Set class
class(pseudo_info) <- "infoCluster"

# Plot
plot(pseudo_info)

# Rotate
plot(pseudo_info, rotate = FALSE)

# Where does Vanderbilt come out?
silhouette_clusters[grepl("Vanderbilt", salary$school_name)]
# Vanderbilt is actually with number 1 in the plot
# There is currently an issue with aligning the original
# clustering numbers to the plotted (it's harder than it seems...)

# OK... but what are the groups?

# Compute your own "centroids"
t(data.frame(
  "Cluster_1" = colMeans(salary_numeric[silhouette_clusters == 1,]),
  "Cluster_2" = colMeans(salary_numeric[silhouette_clusters == 2,])
))

# Is there strong similarity between k-mediods and hierarchical clusters?
## Adjusted Rand Index
compare(
  silhouette_run$clustering, silhouette_clusters,
  method = "adjusted.rand"
)

## Normalized Mutual Information
compare(
  silhouette_run$clustering, silhouette_clusters,
  method = "nmi"
)

## Raw proportion
table(silhouette_run$clustering, silhouette_clusters)
(89 + 222) / (89 + 222 + 3 + 6)

# Quite similar!

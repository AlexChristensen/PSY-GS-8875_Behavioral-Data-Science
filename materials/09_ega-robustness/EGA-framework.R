#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 9: EGA Framework ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load packages
library(EGAnet); library(psychTools); library(ggplot2)

# Obtain data (in {psychTools})
data <- bfi[,1:25]

#%%%%%%%%%%%%%%%%%%%%
## Bootstrap EGA ----
#%%%%%%%%%%%%%%%%%%%%

# Before running the next code, check how many
# processing cores you have on your computer
parallel::detectCores()

# Implement bootstrap EGA (empirical automatically computed)
bfi_boot <- bootEGA(
  data, seed = 42, # many {EGAnet} functions set a seed *internally*
  type = "parametric", # assumes multivariate normal
  ncores = 8 # usually best to do half the number of cores
)

# Print summary
summary(bfi_boot)

# You can also compute
bfi_stability <- dimensionStability(bfi_boot)

# Print summary
summary(bfi_stability)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Unique Variable Analysis ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Apply UVA
bfi_uva <- UVA(
  data, key = as.character(bfi.dictionary$Item[1:25])
  # using a 'key' makes the output more interpretable
)

# Print summary
summary(bfi_uva)

#%%%%%%%%%%%%%%%%%%%%%%%
## Network Loadings ----
#%%%%%%%%%%%%%%%%%%%%%%%

# Apply EGA
bfi_ega <- EGA(data)

# We could have also gotten the empirical
# EGA from our bootstrap EGA
bfi_ega <- bfi_boot$EGA; plot(bfi_ega)

# Compute network loadings
bfi_loadings <- net.loads(bfi_ega, loading.method = "experimental")

# Print summary
summary(bfi_loadings)

# Order output
round(bfi_loadings$std[colnames(bfi_ega$network),], 2)

#%%%%%%%%%%%%%%%%%%%%%%%%%%
## (Metric) Invariance ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%

# Obtain groups
groups <- ifelse(bfi[,"education"] < 4, "Non-grad", "Grad")

# Filter for missing groups
group_data <- data[!is.na(groups),]
groups <- na.omit(groups)

# Frequencies
table(groups)

# Perform metric invariance
bfi_invariance <- invariance(
  data = group_data, groups = groups,
  structure = rep(1:5, each = 5), # theoretical structure
  loading.method = "experimental", # use latest loadings
  ncores = 8, seed = 42
)

# Summary
summary(bfi_invariance)

# Plot
plot(bfi_invariance, p_type = "p_BH")

#%%%%%%%%%%%%%%%%%%%%%%%
## Hierarchical EGA ----
#%%%%%%%%%%%%%%%%%%%%%%%

# Obtain SAPA data
sapa <- psychTools::spi[,11:145]

# Apply hierarchical EGA
sapa_hier <- hierEGA(
  data = sapa,
  loading.method = "experimental",
  scores = "network"
)

# Summary
summary(sapa_hier)

# Plot
plot(sapa_hier, node.size = 4, label.size = 2)

# Apply bootstrap hierarchical EGA
sapa_hier_boot <- bootEGA(
  data = sapa, EGA.type = "hierEGA",
  loading.method = "experimental",
  scores = "network",
  ncores = 8, seed = 42
)
# This procedure will take some time (135 *ordinal* items)
# About 4 minutes on my laptop with 8 cores

# Summary
summary(sapa_hier_boot)

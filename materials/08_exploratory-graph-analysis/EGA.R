#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 8: Exploratory Graph Analysis ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load packages
library(EGAnet); library(psychTools)
library(lavaan); library(semPlot)
library(ggplot2)

# Obtain data (in {psychTools})
data <- bfi[,1:25]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Confirmatory Factor Analysis ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Set up correlated factor model
model <- paste0(
  "O =~ ", paste0("O", 1:5, collapse = " + "), "\n",
  "C =~ ", paste0("C", 1:5, collapse = " + "), "\n",
  "E =~ ", paste0("E", 1:5, collapse = " + "), "\n",
  "A =~ ", paste0("A", 1:5, collapse = " + "), "\n",
  "N =~ ", paste0("N", 1:5, collapse = " + ")
)

# Fit CFA model
fit <- cfa(
  model = model, data = data,
  ordered = colnames(data), # ensure data are treated as ordinal
  estimator = "WLSMV" # use categorical estimator
)

# Summary
round(
  fitMeasures(fit)[c(
    "baseline.chisq.scaled", "baseline.df.scaled",
    "baseline.pvalue.scaled",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled",
    "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled",
    "srmr", "srmr_bentler"
  )], 3
)

# Plot CFA
semPaths(
  fit, what = "std",
  intercepts = FALSE, residuals = FALSE,
  thresholds = FALSE, sizeLat = 7, sizeMan = 5,
  node.width = 1, layout = "circle"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Exploratory Graph Analysis ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Compute correlations
correlations <- auto.correlate(data, verbose = TRUE)

# Visualize
EGAnet:::ggheatmap(correlations, type = "full") +
  geom_tile(color = "grey") +
  scale_fill_gradient2(
    low = "#CD533B", mid = "#EAEBED",
    high = "#588B8B", limits = c(-1, 1),
    guide = guide_colorbar(
      frame.colour = NA,
      ticks.colour = NA
    )
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  labs(title = "Zero-order Correlations") +
  geom_rect( # agreeableness
    aes(xmin = 0.5, xmax = 5.5, ymin = 20.5, ymax = 25.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # conscientiousness
    aes(xmin = 5.5, xmax = 10.5, ymin = 15.5, ymax = 20.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # extraversion
    aes(xmin = 10.5, xmax = 15.5, ymin = 10.5, ymax = 15.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # neuroticism
    aes(xmin = 15.5, xmax = 20.5, ymin = 5.5, ymax = 10.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # openness to experience
    aes(xmin = 20.5, xmax = 25.5, ymin = 0.5, ymax = 5.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  )
# You can ignore the warnings

# Compute partial correlations
## Uses: pcor <- -cov2cor(solve(correlations))
## And sets diagonal to zero: diag(pcor) <- 0
partial <- EGAnet:::cor2pcor(correlations)

# Visualize
EGAnet:::ggheatmap(partial, type = "full") +
  geom_tile(color = "grey") +
  scale_fill_gradient2(
    low = "#CD533B", mid = "#EAEBED",
    high = "#588B8B", limits = c(-1, 1),
    guide = guide_colorbar(
      frame.colour = NA,
      ticks.colour = NA
    )
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  labs(title = "Partial Correlations") +
  geom_rect( # agreeableness
    aes(xmin = 0.5, xmax = 5.5, ymin = 20.5, ymax = 25.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # conscientiousness
    aes(xmin = 5.5, xmax = 10.5, ymin = 15.5, ymax = 20.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # extraversion
    aes(xmin = 10.5, xmax = 15.5, ymin = 10.5, ymax = 15.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # neuroticism
    aes(xmin = 15.5, xmax = 20.5, ymin = 5.5, ymax = 10.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # openness to experience
    aes(xmin = 20.5, xmax = 25.5, ymin = 0.5, ymax = 5.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  )

# Network estimation | On the correlation matrix
bfi_network <- network.estimation(
  data = correlations, n = nrow(data), model = "glasso"
) # needs the number of cases to use correlations

# Network estimation | On the data
bfi_network <- network.estimation(data, model = "glasso")

# Create class to plot
network_class <- list(
  network = bfi_network,
  wc = rep(1, ncol(bfi_network))
)
class(network_class) <- "EGA"
# Creates pseudo-EGA class object for easier plotting

# Plot
plot(
  network_class, title = "BFI network"
) + theme(legend.position = "none")

# Visualize network heatmap
EGAnet:::ggheatmap(bfi_network, type = "full") +
  geom_tile(color = "grey") +
  scale_fill_gradient2(
    low = "#CD533B", mid = "white",
    high = "#588B8B", limits = c(-1, 1),
    guide = guide_colorbar(
      frame.colour = NA,
      ticks.colour = NA
    )
  ) +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  ) +
  labs(title = "EBICglasso Network") +
  geom_rect( # agreeableness
    aes(xmin = 0.5, xmax = 5.5, ymin = 20.5, ymax = 25.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # conscientiousness
    aes(xmin = 5.5, xmax = 10.5, ymin = 15.5, ymax = 20.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # extraversion
    aes(xmin = 10.5, xmax = 15.5, ymin = 10.5, ymax = 15.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # neuroticism
    aes(xmin = 15.5, xmax = 20.5, ymin = 5.5, ymax = 10.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  ) +
  geom_rect( # openness to experience
    aes(xmin = 20.5, xmax = 25.5, ymin = 0.5, ymax = 5.5),
    inherit.aes = FALSE, fill = NA, color = "black",
    linewidth = 0.25
  )

# Apply Walktrap algorithm
bfi_walktrap <- community.detection(bfi_network, algorithm = "walktrap")

# Print summary
summary(bfi_walktrap)

# Update pseudo-EGA class object
network_class$wc <- bfi_walktrap

# Plot
plot(network_class)

# Apply Louvain algorithm
bfi_louvain <- community.detection(bfi_network, algorithm = "louvain")

# Print summary
summary(bfi_louvain)

# Update pseudo-EGA class object
network_class$wc <- bfi_louvain

# Plot
plot(network_class)

# Apply EGA
bfi_ega <- EGA(data, plot.EGA = FALSE)
# You can adjust the 'algorithm' argument to
# any algorithm in `community.detection`

# Print summary
summary(bfi_ega)
# Note that unidimensionality *and* TEFI is already included

# Plot
plot(bfi_ega)
# By default, `plot.EGA = TRUE` and will automatically
# plot the network when using `EGA`

# Apply unidimensional
community.unidimensional(data[,grep("O", colnames(data))])

# Apply EGA + TEFI with Walktrap
bfi_walktrap_fit <- EGA.fit(data, algorithm = "walktrap")

# Print summary
summary(bfi_walktrap_fit)

# Apply EGA + TEFI with Louvain
bfi_louvain_fit <- EGA.fit(data, algorithm = "louvain")
# Takes a bit longer than Walktrap (searching over 25x more parameter values)

# Print summary
summary(bfi_louvain_fit)
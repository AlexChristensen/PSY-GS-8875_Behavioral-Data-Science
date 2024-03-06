#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 6: Trees and Forests ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load packages
library(caret); library(ggplot2)
library(ranger); library(ggpubr)
library(rpart); library(rpart.plot)

# Load data
schizotypy <- read.csv("../../data/schizotypy/share_430n_interview.csv")

# Wisconsin Schizotypy Scales
# py = physical anhedonia
# sa = social anhdenoia
# pb = perception aberration
# mi = magical ideation

# Outcomes of interest
# gas = global functioning (0-100; higher = better)
# psx_high = psychotic-like experiences (positive symptoms)
# nsm_tot = negative symptoms

# Fill in `NA` with 0
schizotypy[,4:63][is.na(schizotypy[,4:63])] <- 0

# Histogram of functioning and symptoms
function_df <- data.frame(
  outcome = rep(
    c("Functioning", "Positive", "Negative"),
    each = nrow(schizotypy)
  ),
  values = c(
    schizotypy$gas, schizotypy$psx_high,
    schizotypy$nsm_tot
  )
)

# Visualize
ggplot(
  data = function_df,
  aes(x = values, fill = outcome)
) +
  geom_density(alpha = 0.60) +
  geom_point(
    aes(y = -0.05), position = position_jitter(height = 0.025),
    shape = 21, color = "white", alpha = 0.60
  ) +
  geom_boxplot(
    aes(y = -0.05), width = 0.05, outlier.shape = NA,
    fill = NA
  ) +
  facet_wrap(~outcome, nrow = 3, scales = "free") +
  labs(x = "Values", y = "Density") +
  scale_x_continuous(n.breaks = 8) +
  theme(
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.line = element_line(linewidth = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_fill_manual(
    values = c("#533745", "#AB4E68", "#B07156")
  )

# For color palettes, check out: https://coolors.co

# Check out tables for negative and positive symptoms
table(schizotypy$nsm_tot > 4)
quantile(schizotypy$nsm_tot, probs = c(0.75, 0.95))
table(schizotypy$psx_high > 2)
quantile(schizotypy$psx_high, probs = c(0.75, 0.95))

# How many are the same?
sum(schizotypy$nsm_tot > 4 & schizotypy$psx_high > 2)

# Set up group variables
schizotypy$negative <- factor(
  as.numeric(schizotypy$nsm_tot > 4), levels = c(0, 1)
)
schizotypy$positive <- factor(
  as.numeric(schizotypy$psx_high > 2), levels = c(0, 1)
)

# Check out quantile for general functioning
quantile(schizotypy$gas, probs = c(
  0.025, 0.05, 0.10, 0.90, 0.95, 0.975
))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Negative classification ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Logistic regression
neg_log <- glm(
  negative ~ ., data = schizotypy[,c(4:63, 74)],
  family = "binomial"
); summary(neg_log)

# Get predictions
neg_probs <- predict(neg_log, type = "response")
neg_class <- factor(ifelse(neg_probs > 0.50, 1, 0), levels = c(0, 1))

# Check confusion matrix
confusionMatrix(
  data = neg_class,
  reference = schizotypy$negative,
  positive = "1"
)

# Perform tree
negative_tree <- rpart(
  negative ~ ., data = schizotypy[,c(4:63, 74)],
  parms = list(split = "gini")
)

# Identify best cp value
negative_tree$cptable[
  which.min(negative_tree$cptable[,"rel error"]),
  "CP" # find lowest error cp
]

# Prune tree
negative_prune <- prune(negative_tree, cp = 0.01)

# Plot
prp(negative_prune, extra = 1)

# Using rpart.plot
rpart.plot(
  negative_prune, extra = 104,
  box.palette = "GnBu", # color scheme
  branch.lty = 3, # dotted branch lines
  shadow.col = "gray", # shadows under the node boxes
  nn = TRUE
)
# Top node
## 0 = predicted class
## 0.73 proportion saying "yes" to SA07 = 0
## 0.23 proportion saying "no" to SA07 = 0 (or SA07 = 1)
## Percentage of observations in node (100%)
## Left = yes; right = no

# Next level
## Left
## 0 = predicted class
## 0.83 proportion saying "yes" to PY01 = 0
## 0.17 proportion saying "no" to PY01 = 0 (or PY01 = 1)
## Percentage of observations in node (74%)
## Left = yes; right = no

## Right
## 1 = predicted class
## 0.54 proportion saying "yes" to SA09 = 0
## 0.46 proportion saying "no" to SA09 = 0 (or SA09 = 1)
## Percentage of observations in node (26%)
## Left = yes; right = no

# Prediction
tree_probabilities <- predict(negative_tree)
tree_class <- factor(
  as.numeric(tree_probabilities[,2] > tree_probabilities[,1]),
  levels = c(0, 1)
)

# Check confusion matrix
confusionMatrix(
  data = tree_class,
  reference = schizotypy$negative,
  positive = "1"
)

# Set seed for reproducibility
set.seed(42)

# Random forest cross-validation for parameters
tictoc::tic()
store_caret <- train(
  x = schizotypy[,4:63],
  y = schizotypy$negative,
  method = "ranger",
  metric = "Kappa", # better for imbalanced datasets
  trControl = trainControl(
    method = "cv", number = 5
    #, sampling = "smote"
    # For class imbalances:
    # https://topepo.github.io/caret/subsampling-for-class-imbalances.html
  ),
  tuneGrid = expand.grid(
    mtry = seq(1, 60, 1), # 1:ncol(data)
    min.node.size = seq(1, 10, 1), # 1-10 is usually good
    splitrule = "gini" # classification
  ),
  num.trees = 500, # keep at 500 for the initial search
  importance = "impurity" # set up for later
); store_caret
tictoc::toc()
# This process will take a long time
# On my laptop (16 cores): 83.822 sec elapsed

# With the `mtry` and `min.node.size` parameters,
# search over `num.trees`
trees <- c(10, 50, 100, 250, 500, 1000, 1500)

# Store results
results <- vector("list", length(trees))

# Perform cross-validation (will be much faster than before)
for(i in seq_along(trees)){

  # Perform
  results[[i]] <- train(
    x = schizotypy[,4:63],
    y = schizotypy$negative,
    method = "ranger",
    metric = "Kappa", # better for unbalanced datasets
    trControl = trainControl(
      method = "cv", number = 5
      #, sampling = "smote"
      # For class imbalances:
      # https://topepo.github.io/caret/subsampling-for-class-imbalances.html
    ),
    tuneGrid = data.frame(
      mtry = 43, splitrule = "gini",
      min.node.size = 9
    ),
    num.trees = trees[i], # search over trees
    importance = "impurity" # set up for later
  )$results

}

# Combine results
combined <- do.call(rbind.data.frame, results)
combined$num.trees <- trees
print(combined, digits = 4)

# 250 trees has the best accuracy/kappa

# Get final model
ranger_model <- ranger(
  formula = negative ~ .,
  data = schizotypy[,c(4:63, 74)],
  mtry = 43, splitrule = "gini", # 43, splitrule = "gini",
  min.node.size = 9, num.trees = 250,
  importance = "impurity", seed = 42 # for reproducibility
)

# Get predictions
ranger_predictions <- predict(ranger_model, data = schizotypy)$predictions

# Check confusion matrix
confusionMatrix(
  data = ranger_predictions,
  reference = schizotypy$negative,
  positive = "1"
)

# Check out importance
ranger_imp <- importance(ranger_model); ranger_imp

# Visualize importance
ggdotchart(
  data = data.frame(
    Importance = round(ranger_imp, 2),
    Variable = names(ranger_imp),
    Dimension = rep(
      c(
        "Physical Anhedonia", "Perceptual Aberration",
        "Magical Ideation", "Social Anhedonia"
      ), each = 15
    )
  ),
  x = "Variable", y = "Importance", color = "Dimension",
  dot.size = 5, add = "segments", label = "Importance",
  group = "Dimension", # for within-dimension comparison
  font.label = list(size = 8, vjust = 0.5, color = "black", face = "bold")
) +
  scale_y_continuous(limits = c(0, 15), n.breaks = 8, expand = c(0, 0)) +
  guides(color = guide_legend(title.position = "left", nrow = 2)) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14, hjust = 0.5),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  coord_flip()

# Look at first tree
treeInfo(ranger_model, tree = 1)

# For extra interpretability
library(randomForestExplainer)

# Explain forest
explain_forest(
  ranger_model, data = schizotypy,
  path = paste0(getwd(), "/schizotypy_negative")
)



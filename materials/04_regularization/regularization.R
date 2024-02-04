#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 3: Regularization ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load packages
library(glmnet); library(selectiveInference)
library(MASS); library(ggplot2)

# Set seed for reproduciblity
set.seed(42)

# Load data
load("../../data/ncds/ncds_sample.RData")

# Set up our X and Ys
X <- as.matrix(ncds_sample[,2:51]) # 50 personality items
Y <- as.matrix(ncds_sample[,"wem_well_being"]) # well-being

# Update X and Y
keep <- complete.cases(X) & !is.na(Y)
X <- X[keep,]; Y <- Y[keep,, drop = FALSE]

#%%%%%%%%%%%%%%%%%%%%%%%
## Ridge regression ----
#%%%%%%%%%%%%%%%%%%%%%%%

# Set lambda
lambda <- 10

# Compute ridge regression
fit <- glmnet(
  x = X, y = Y, family = "gaussian",
  alpha = 0, # 0 = ridge; 1 = lasso
  lambda = lambda, # penalty parameter
  standardize = TRUE
)

# Print coefficients
net <- coef(fit)
net

# Coefficients scaled
net_scaled <- coef(
  fit, x = X, y = Y, exact = TRUE,
  s = lambda / length(Y)
)
net_scaled

# Compare with {MASS} implementation
mass <- coef(lm.ridge(Y ~ X, lambda = 10))
mass

# Closed-form solution

# Set up X with intercept
X_intercept <- cbind(1, X)

# Set up penalization matrix
lambda <- 10
ridge_matrix <- lambda * diag(ncol(X_intercept))
ridge_matrix[1, 1] <- 0 # don't regularize the intercept

# Matrix algebra computation
analytical <- solve(
  crossprod(X_intercept) + ridge_matrix
) %*% crossprod(X_intercept, Y)
analytical

# Compare solutions (use [-1] to remove intercept)
mean(abs(mass - analytical)[-1])
range(abs(mass - analytical)[-1])
mean(abs(mass - net)[-1])
range(abs(mass - net)[-1])

# What's happening here?
# Check out: https://stats.stackexchange.com/questions/129179/why-is-glmnet-ridge-regression-giving-me-a-different-answer-than-manual-calculat
# {glmnet} uses a scaling factor on the penalty: sd(Y) / length(Y)

# Compare solutions
mean(abs(mass - net_scaled)[-1])
range(abs(mass - net_scaled)[-1])

# Compare to linear model
linear <- coef(lm(Y ~ X))
mean(abs(mass - linear)[-1])
range(abs(mass - linear)[-1])

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Plot demonstration of lambda ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Initialize lambdas
lambdas <- seq(0, 250, exp(1))

# Compute coefficients over many lambda
coefficients <- lapply(
  lambdas, function(lambda){

    # Compute ridge regression
    fit <- glmnet(
      x = X, y = Y, family = "gaussian",
      alpha = 0, # 0 = ridge, 1 = lasso
      lambda = lambda, # penalty parameter
      standardize = TRUE
    )

    # Return coefficients
    return(as.vector(coef(fit)))

  }
)

# Set up data frame to plot
coef_df <- do.call(
  what = rbind.data.frame,
  args = lapply(seq_along(lambdas), function(index){

    data.frame(
      lambda = lambdas[[index]],
      item = colnames(X),
      coefficient = coefficients[[index]][-1]
    )

  })
)

# Plot
ridge_lambdas <- ggplot(
  data = coef_df, aes(
    x = lambda, y = coefficient,
    group = item, color = item
  )
) + geom_line() +
  theme_minimal() +
  labs(x = expression(lambda), y = expression(beta[~~ridge])) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.line = element_line(linewidth = 0.5),
    legend.position = "none"
  ); ridge_lambdas

# Save plot
ggsave(
  ridge_lambdas, filename = "../images/ridge_lambdas.png",
  dpi = 600, height = 4, width = 6
)

#%%%%%%%%%%%%%%%%%%%%%%%
## LASSO regression ----
#%%%%%%%%%%%%%%%%%%%%%%%

# Plot demonstration of lambda

# Initialize lambdas
## Notice that the same scaling of values
## is not used (LASSO has a "harsher" penalty)
lambdas <- seq(0, 5, exp(-2))

# Compute coefficients over many lambda
coefficients <- lapply(
  lambdas, function(lambda){

    # Compute ridge regression
    fit <- glmnet(
      x = X[,-1], y = Y, family = "gaussian",
      alpha = 1, # 0 = ridge, 1 = lasso
      lambda = lambda # penalty parameter
    )

    # Return coefficients
    return(as.vector(coef(fit)))

  }
)

# Set up data frame to plot
coef_df <- do.call(
  what = rbind.data.frame,
  args = lapply(seq_along(lambdas), function(index){

    data.frame(
      lambda = lambdas[[index]],
      item = colnames(X)[-1],
      coefficient = coefficients[[index]][-1]
    )

  })
)

# Plot
lasso_lambdas <- ggplot(
  data = coef_df, aes(
    x = lambda, y = coefficient,
    group = item, color = item
  )
) + geom_line() +
  theme_minimal() +
  labs(x = expression(lambda), y = expression(beta[~~LASSO])) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.line = element_line(linewidth = 0.5),
    legend.position = "none"
  )

# Save plot
ggsave(
  lasso_lambdas, filename = "../images/lasso_lambdas.png",
  dpi = 600, height = 4, width = 6
)
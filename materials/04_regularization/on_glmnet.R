#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 3: On {glmnet} ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load packages
library(glmnet)

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

# Add intercept
X_intercept <- cbind(1, X)

# Set up penalization matrix
lambda <- 10
ridge_matrix <- lambda * diag(ncol(X_intercept))
ridge_matrix[1, 1] <- 0 # don't regularize the intercept

# Matrix algebra computation
analytical <- solve(
  crossprod(X_intercept) + ridge_matrix
) %*% crossprod(X_intercept, Y)

# Get analytic unstandardized coefficients
analytical_unstandardized <- tail(analytical)
analytical_unstandardized

# Compute ridge regression
glmnet_unstandardized <- glmnet(
  x = X, y = Y, family = "gaussian",
  alpha = 0, # 0 = ridge; 1 = lasso
  lambda = 10, # penalty parameter
  thresh = 1e-20, # high precision
  standardize = FALSE
)

# Get {glmnet} unstandardized coefficients
glmnet_unstd_coefs <- tail(as.matrix(coef(glmnet_unstandardized)))
glmnet_unstd_coefs

# Compare coefficients
cbind(analytical_unstandardized, glmnet_unstd_coefs)

# Transform {glmnet} into same scale as analytical solution
tail(as.matrix(coef(
  glmnet_unstandardized,
  x = X, y = Y,
  s = sd(Y) * 10 / nrow(Y), # use lambda
  exact = TRUE
)))
# Closer!

# Use {glmnet}'s standard deviation
glmnet_sd <- sqrt(sum((Y - mean(Y))^2) / nrow(Y))

# Get scaled coefficients
glmnet_unstd_coefs <- tail(as.matrix(coef(
  glmnet_unstandardized,
  x = X, y = Y,
  s = glmnet_sd * 10 / nrow(Y), # use lambda
  exact = TRUE
)))

# Compare coefficients
cbind(analytical_unstandardized, glmnet_unstd_coefs)

# Scale X and Y
X_scaled <- scale(X)
Y_scaled <- scale(Y)

# Add intercept
X_scaled_intercept <- cbind(1, X_scaled)

# Matrix algebra computation
analytical_scaled <- solve(
  crossprod(X_scaled_intercept) + ridge_matrix # doesn't change
) %*% crossprod(X_scaled_intercept, Y_scaled)

# Get analytic standardized coefficients
analytical_standardized <- tail(analytical_scaled)
analytical_standardized

# Compute ridge regression
glmnet_standardized <- glmnet(
  x = X, y = Y, family = "gaussian",
  alpha = 0, # 0 = ridge; 1 = lasso
  lambda = 10, # penalty parameter
  thresh = 1e-20, # high precision accuracy
  standardize = TRUE # default
)

# Get {glmnet} standardized coefficients
glmnet_std_coefs <- tail(as.matrix(coef(glmnet_standardized)))
glmnet_std_coefs
# Here we go again...

# Get scaled coefficients
tail(as.matrix(coef(
  glmnet_standardized,
  x = X_scaled, y = Y_scaled,
  s = 10 / nrow(Y), # use lambda
  exact = TRUE
)))
# Closer!

# Changing the scale of Y will get them to match...

# Scale X and Y
Y_scaled_glmnet <- (Y - mean(Y)) / glmnet_sd

# Matrix algebra computation
analytical_scaled <- solve(
  crossprod(X_scaled_intercept) + ridge_matrix # doesn't change
) %*% crossprod(X_scaled_intercept, Y_scaled_glmnet)

# Get analytic standardized coefficients
analytical_standardized <- tail(analytical_scaled)
analytical_standardized

# Compute ridge regression
glmnet_standardized <- glmnet(
  x = X_scaled, y = Y_scaled_glmnet, family = "gaussian",
  alpha = 0, # 0 = ridge; 1 = lasso
  lambda = 10, # penalty parameter
  thresh = 1e-20, # high precision accuracy
  standardize = FALSE # default
)

# Get {glmnet} standardized coefficients
glmnet_std_coefs <- tail(as.matrix(coef(glmnet_standardized)))
glmnet_std_coefs
# Here we go again...

# Get scaled coefficients
tail(as.matrix(coef(
  glmnet_standardized,
  x = X_scaled, y = Y_scaled_glmnet,
  s = 10 / nrow(Y), # use lambda
  exact = TRUE
)))


# Now, they match...

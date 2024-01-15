#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 2: Regression ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Install {latentFactoR} from GitHub
remotes::install_github("AlexChristensen/latentFactoR")

# Load packages
library(ggplot2); library(latentFactoR)

# Set seed to ensure reproducible results
set.seed(42)

#%%%%%%%%%%%%%%%%%%%%
## Generate data ----
#%%%%%%%%%%%%%%%%%%%%

# Generate Y data that is related to X
X <- matrix(runif(1000, -5, 5))
Y <- matrix(X + rnorm(1000) + 3)

#%%%%%%%%%%%%%%%%%%%%%%%%
## Linear regression ----
#%%%%%%%%%%%%%%%%%%%%%%%%

# Check correlation
cor(X, Y)

# Set up intercept for X
X <- cbind(1, X)

# Perform OLS
solve(crossprod(X)) %*%
crossprod(X, Y)

# `crossprod` = X %*% t(X)

# Compare OLS with base R function
coef(lm(Y ~ X[,-1])) # remove intercept

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Gradient descent style ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# With gradient descent, we need to define
# a cost function to minimize (or maximize)

# Set learning rate
alpha <- 0.01

# Set maximum iterations
max_iter <- 1000

# Initialize beta values
beta <- matrix(c(0, 0))

# Initialize cost and beta histories
cost_history <- numeric(max_iter)
beta_history <- vector("list", length = max_iter)

# Initialize predicted data list
predicted_data <- beta_history

# Perform gradient descent
for(i in seq_len(max_iter)){

  # Compute error
  error <- X %*% beta - Y

  # Compute change
  delta <- crossprod(X, error) / length(Y)

  # Compute new betas
  beta <- beta - alpha * delta

  # Store betas
  beta_history[[i]] <- beta

  # Store predicted data
  predicted_data[[i]] <- data.frame(
    Iteration = i,
    X = X[,2],
    Y_hat = X %*% beta
  )

  # Store cost
  cost_history[i] <- sum(error^2)

}

# Set up data frame for data points
data_df <- data.frame(X = X[,2], Y = Y)

# Set up data frame for beta lines
beta_df <- do.call(rbind.data.frame, predicted_data)

# Plot it out
ggplot() +
  labs(x = "X", y = "Y", title = "Gradient Descent for Linear Regression") +
  geom_line( # add lines
    data = beta_df,
    aes(x = X, y = Y_hat, group = Iteration),
    alpha = 0.1
  ) +
  geom_point( # add points
    data = data_df,
    aes(x = X, y = Y),
    shape = 21
  ) +
  geom_smooth( # standard `lm`
    data = data_df,
    aes(x = X, y = Y), se = FALSE,
    method = "lm", formula = y ~ x
  ) +
  geom_abline(
    aes(
      intercept = beta_history[[which.min(cost_history)]][1,],
      slope = beta_history[[which.min(cost_history)]][2,]
    ), color = "orange", linetype = "longdash"
  ) +
  theme_minimal()

# Find which iteration
which.min(cost_history)

# What are the betas?
beta_history[[which.min(cost_history)]]

# Recall:
# lm(formula = Y ~ X[, -1])
#
# Coefficients:
# (Intercept)     X[, -1]
#  2.970053    1.013516

# Set up cost data frame
cost_df <- data.frame(
  Iteration = seq_along(cost_history),
  SSE = cost_history
)

# Plot the cost
ggplot(data = cost_df, aes(x = Iteration, y = SSE)) +
  labs(title = "Cost for Linear Regression") +
  geom_point(shape = 21) +
  theme_minimal()

# Could increase the learning rate OR
# number of iterations to minimize cost further

#%%%%%%%%%%%%%%%%
## Your turn ----
#%%%%%%%%%%%%%%%%

# Your goal is to "fine-tune" the gradient descent algorithm
# to get the beta coefficients to match the coefficients
# determine by OLS

# Here's your data
set.seed(42)

# Generate data
X <- rnorm(1000, mean = 0, sd = 0.10)
Y <- X + rnorm(1000)

# OLS
lm(Y ~ X)

# Source the gradient descent functon
source("./source/glm_gradient_descent.R")

# Arguments
# X = predictors (intercept not needed)
# Y = outcome
# learning_rate = steepness of descent (default = 0.01)
# max_iter = maximum number of iterations (default = 1000)
# activation = linear or sigmoid (default = "linear")

# Adjust the parameters of the gradient descent
# to get near the OLS output
glm_gradient_descent(
  X, Y, learning_rate = 0.01,
  max_iter = 1000, activation = "linear"
)

# Perform OLS
coef(lm(Y ~ X))

#%%%%%%%%%%%%%%%%%%%%%%%%%%
## Logistic regression ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%

# Convert Y to binary
Y <- categorize(Y, 2) - 1

# Adjust the parameters of the gradient descent
# to get near the logistic regression output
glm_gradient_descent(
  X, Y, learning_rate = 0.01,
  max_iter = 1000, activation = "sigmoid"
)

# Logistic regression
coef(glm(Y ~ X, family = "binomial"))

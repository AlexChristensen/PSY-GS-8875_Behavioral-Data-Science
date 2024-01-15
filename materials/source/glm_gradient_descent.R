# Function for linear regression gradient descent
glm_gradient_descent <- function(
    X, Y, learning_rate = 0.01, max_iter = 1000,
    activation = c("linear", "sigmoid")
)
{

  # Default to linear activation
  if(missing(activation)){
    activation <- "linear"
  }else{activation <- match.arg(activation)}

  # Ensure matrices
  X <- as.matrix(X); Y <- as.matrix(Y)

  # Check for whether intercept exists
  if(any(X[,1] != 1)){
    X <- cbind(1, X)
  }

  # Get dimensions
  dimensions <- dim(X)

  # Initialize beta values
  beta <- matrix(rep(0, dimensions[2]))

  # Initialize value store
  store_values <- matrix(
    NA, nrow = max_iter, ncol = dimensions[2] + 2,
    dimnames = list(
      NULL,
      c("iteration", "cost", paste0("beta_", 0:(dimensions[2] - 1)))
    )
  )

  # Perform gradient descent
  for(i in seq_len(max_iter)){

    # Compute values
    values <- switch(
      activation,
      "linear" = X %*% beta,
      "sigmoid" = 1 / (1 + exp(-(X %*% beta)))
    )

    # Compute error
    error <- values - Y

    # Compute change
    delta <- crossprod(X, error) / dimensions[1]

    # Compute new betas
    beta <- beta - learning_rate * delta

    # Compute cost
    cost <- switch(
      activation,
      "linear" = sum(error^2),
      "sigmoid" = sum(-((Y * log(values)) + ((1 - Y) * log(1 - values))))
      # binary cross-entropy loss
    )

    # Store values
    store_values[i,] <- c(i, cost, beta)

  }

  # Return list
  return(
    list(
      results = store_values,
      best_result = store_values[which.min(store_values[,"cost"]),]
    )
  )

}

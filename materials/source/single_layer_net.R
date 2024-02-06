#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Functions for Neural Network from Scratch ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Credit to Akshaj Verma
# Adapted from:
# https://rviews.rstudio.com/2020/07/24/building-a-neural-net-from-scratch-using-r-part-1/
# https://rviews.rstudio.com/2020/07/24/building-a-neural-net-from-scratch-using-r-part-2/

# Steps
# 1. Initialize parameters
# 2. Forward propagation
# 3. Cost
# 4. Backward propagation
# 5. Update parameters
# 6. Repeat 2-5 until iterations or convergence

# Train network function
train_net <- function(X, y, iterations, hidden_neurons, learning_rate){

  # Ensure X and y are matrices
  X <- data.matrix(X)
  y <- data.matrix(as.numeric(as.character(y)))

  # 1. Initialize parameters
  parameters <- initialize_parameters(X, y, hidden_neurons)

  # Initialize cost to save
  cost_history <- numeric(iterations)

  # Set up incremental updates
  update <- round(seq(1, iterations, length.out = 10))

  # Set previous cost
  previous_cost <- Inf

  # 6. Repeat 2-5 until iterations or convergence
  for(i in seq_len(iterations)){

    # 2. Forward propagation
    fp_output <- forward_propagation(X, parameters)

    # 3. Compute cost (binary cross-entropy)
    cost <- computeCost(y, fp_output$A2)

    # 4. Backward propagation (backprop)
    bp_output <- backpropagation(X, y, fp_output, parameters, hidden_neurons)

    # 5. Update parameters
    parameters <- update_parameters(bp_output, parameters, learning_rate)

    # Check for update
    if(i %in% update){
      cat("Iteration: ", i, " | Cost: ", cost, "\n")
    }

    # Check for best parameters
    if(cost < previous_cost){

      # Update best cost and parameters
      best_cost <- cost
      best_parameters <- parameters
      best_iteration <- i

      # Update previous best
      previous_cost <- cost

    }

    # Add to cost history
    cost_history[i] <- cost

  }

  # Set up results
  results <- list(
    best = list(
      cost = best_cost,
      parameters = best_parameters,
      iteration = best_iteration
    ),
    last = list(
      cost = cost,
      parameters = parameters
    ),
    cost_history = cost_history
  )

  # Set class for S3 method `predict`
  class(results) <- "neural_net"

  # Return results
  return(results)

}

# 1. Initialize parameters
initialize_parameters <- function(X, y, hidden_neurons){

  # Get number of variables
  variables <- dim(X)[2]

  # Get number of output
  output <- dim(y)[2]

  # Return initialization
  return(
    list(
      "W1" = matrix(
        runif(variables * hidden_neurons), nrow = variables,
        ncol = hidden_neurons, byrow = TRUE
      ) * 0.01,
      "b1" = matrix(rep(0, hidden_neurons), ncol = hidden_neurons),
      "W2" = matrix(
        runif(output * hidden_neurons), nrow = hidden_neurons,
        ncol = output, byrow = TRUE
      ) * 0.01,
      "b2" = matrix(rep(0, output), ncol = output)
    )
  )

}

# Sigmoid activation function
sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}

# 2. Forward propagation
forward_propagation <- function(X, parameters){

  # Compute output from input layer through hidden layer
  Z1 <- sweep(
    X %*% parameters$W1, # matrix multiply data by weights
    MARGIN = 2, STATS = parameters$b1, # add bias (intercept)
    FUN = "+"
  )

  # Apply activation function to output
  A1 <- sigmoid(Z1)

  # Compute output from hidden layer to output layer
  Z2 <- sweep(
    A1 %*% parameters$W2, # matrix multiply data by weights
    MARGIN = 2, STATS = parameters$b2, # add bias (intercept)
    FUN = "+"
  )

  # Apply activation function to output
  A2 <-  sigmoid(Z2)

  # Return parameters
  return(
    list(
      Z1 = Z1, A1 = A1,
      Z2 = Z2, A2 = A2
    )
  )

}

# 3. Cost function
computeCost <- function(y, y_hat){

  # Return binary cross-entropy (add small weight to avoid zeros)
  y_hat <- y_hat + 1e-10
  return(
    -mean((y * log(y_hat)) + ((1 - y) * log(1 - y_hat)))
  )

}

# 4. Backward propagation
backpropagation <- function(X, y, fp_output, parameters, hidden_neurons){

  # Get number of cases
  cases <- dim(X)[1]

  # Get number of output
  output <- dim(y)[2]

  # Get inverse of cases
  inv_cases <- 1 / cases

  # Difference between prediction and actual (error)
  dZ2 <- fp_output$A2 - y

  # Compute difference for output layer to hidden layer weights
  dW2 <- inv_cases * crossprod(dZ2, fp_output$A1)

  # Compute difference for output layer to hidden layer biases
  db2 <- matrix(inv_cases * sum(dZ2), ncol = output)

  # Difference between hidden layer and output from input layer
  dZ1 <- tcrossprod(dZ2, parameters$W2) * (fp_output$A1 * (1 - fp_output$A1))

  # Compute difference for hidden layer to input layer weights
  dW1 <- inv_cases * crossprod(dZ1, X)

  # Compute difference for hidden layer to input layer biases
  db1 <- matrix(inv_cases * colSums(dZ1), nrow = hidden_neurons)

  # Send back gradients
  return(
    list(
      "dW1" = dW1, "db1" = db1,
      "dW2" = dW2, "db2" = db2
    )
  )

}

# 5. Update parameters
update_parameters <- function(bp_output, parameters, learning_rate){

  # Return updated parameters
  return(
    list(
      "W1" = parameters$W1 - learning_rate * t(bp_output$dW1),
      "b1" = parameters$b1 - learning_rate * t(bp_output$db1),
      "W2" = parameters$W2 - learning_rate * t(bp_output$dW2),
      "b2" = parameters$b2 - learning_rate * bp_output$db2
    )
  )

}

# Function to predict new data
predict.neural_net <- function(model, newdata){

  # Extract best network parameters
  best_parameters <- model$best$parameters

  # Match intersecting names
  matched_names <- intersect(row.names(best_parameters$W1), colnames(newdata))

  # Get specific data to match weights
  specific_data <- data.matrix(newdata[,matched_names])

  # Feed through forward propagation
  return(forward_propagation(specific_data, best_parameters)$A2)

}

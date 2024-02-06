#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 5: Neural Networks ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load packages
library(ggplot2); library(ggpubr)

# Generate data
set.seed(1)

# Number of data points
cases <- 400
half_cases <- cases / 2

# Initialize data (2 variables)
X <- matrix(0, nrow = cases, ncol = 2)
Y <- matrix(0, nrow = cases, ncol = 1)

# Loop over to generate data
for(i in 0:1){
  index <- (half_cases * i) + 1:half_cases
  values <- seq(i * 3.12, (i + 1) * 3.12, length.out = half_cases) + rnorm(half_cases, sd = 0.20)
  multiplier <- 4 * sin(4 * values) + rnorm(half_cases, sd = 0.20)
  X[index, 1] <- multiplier * sin(values)
  X[index, 2] <- multiplier * cos(values)
  Y[index,] <- i
}

# Set up as data frame
df <- as.data.frame(cbind(X, Y))
colnames(df) <- c("X1", "X2", "Y")
df$Y <- factor(df$Y, levels = c(0, 1))

# Get training and testing data
train <- sample(1:nrow(df), round(nrow(df) * 0.80))
df_train <- df[train,]
df_test <- df[-train,]

# Check out frequencies (for balances)
table(df_train$Y)
table(df_test$Y)

# Scale values (super important!!)

## Get scaling of train values
centers <- colMeans(df_train[,-3])
scales <- apply(df_train[,-3], 2, sd)
## Keep these values for ALL future testing and generalization!!

## Create function to apply scale to train and test
train_scale <- function(data, centers, scales){

  # 1. Center
  centered <- sweep(
    x = data, MARGIN = 2,
    STATS = centers, FUN = "-"
  )

  # 2. Scale
  scaled <- sweep(
    x = centered, MARGIN = 2,
    STATS = scales, FUN = "/"
  )

  # Return
  return(scaled)

}

## Finally, scale your data
df_train[,-3] <- train_scale(df_train[,-3], centers, scales)
df_test[,-3] <- train_scale(df_test[,-3], centers, scales)

# Ensures that any future data will be scaled in the *exact*
# same way that the original data were scaled

# Visualize data
function_plot <- ggplot(data = df_train, aes(x = X1, y = X2, color = Y)) +
  geom_point(shape = 21, size = 2.5, stroke = 1.5) +
  theme_minimal() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, hjust = 0.5),
    legend.text = element_text(size = 12, hjust = 0.5)
  ); function_plot

# Perform logistic regression
log_train <- glm(Y ~ ., data = df_train, family = "binomial")

# Get class predictions
log_prob_train <- predict(log_train, type = "response")
log_class_train <- ifelse(log_prob_train > 0.50, 1, 0)

# Accuracy
mean(log_class_train == df_train$Y)

# Create a grid (to visualize decision boundary)
X1_range <- seq(
  from = min(df_train$X1, df_test$X1),
  to = max(df_train$X1, df_test$X1),
  length.out = 150
)
X2_range <- seq(
  from = min(df_train$X2, df_test$X2),
  to = max(df_train$X2, df_test$X2),
  length.out = 150
)
grid <- expand.grid(X1 = X1_range, X2 = X2_range)

# Predict using the neural network for each point in the grid
grid_predictions <- predict(log_train, grid, type = "response")

# Convert predictions to classes
grid_class <- data.frame(
  X1 = grid$X1,
  X2 = grid$X2,
  Y = factor(
    ifelse(grid_predictions > 0.5, 1, 0),
    levels = c(0, 1)
  )
)

# Visualize data (with decision boundary)
log_train_plot <- ggplot(data = df_train, aes(x = X1, y = X2, color = Y)) +
  geom_point(
    data = grid_class, aes(x = X1, y = X2, color = Y),
    size = 1, shape = 21, alpha = 0.5
  ) +
  geom_point(
    data = df_train, aes(x = X1, y = X2, color = Y),
    size = 2.5
  ) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(
    title = "Logistic Regression Decision Boundary",
    subtitle = "Training Data"
  ) +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ); log_train_plot

# Check testing accuracy
log_prob_test <- predict(log_train, type = "response", newdata = df_test)
log_class_test <- ifelse(log_prob_test > 0.50, 1, 0)

# Accuracy
mean(log_class_test == df_test$Y)

# Visualize data
log_test_plot <- ggplot(data = df_test, aes(x = X1, y = X2, color = Y)) +
  geom_point(
    data = grid_class, aes(x = X1, y = X2, color = Y),
    size = 1, shape = 21, alpha = 0.5
  ) +
  geom_point(
    data = df_test, aes(x = X1, y = X2, color = Y),
    size = 2.5
  ) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(
    title = "Logistic Regression Decision Boundary",
    subtitle = "Testing Data"
  ) +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ); log_test_plot

# Combine plots
log_plot <- ggarrange(
  log_train_plot, log_test_plot,
  nrow = 1
)

#%%%%%%%%%%%%%%%%%%%%%
## Neural network ----
#%%%%%%%%%%%%%%%%%%%%%

# Load functions
source("../source/single_layer_net.R")

# Set seed
set.seed(42)

# Neural network
network_train <- train_net(
  X = df_train[,c("X1", "X2")], y = df_train[,"Y"],
  iterations = 100000, hidden_neurons = 6,
  learning_rate = 0.05
)

# Plot cost
plot(network_train$cost_history, type = "l")

# Get predictions
network_prob_train <- predict(network_train, newdata = df_train)
network_class_train <- ifelse(network_prob_train > 0.50, 1, 0)

# Accuracy
mean(network_class_train == df_train$Y)

# Create a grid (to visualize decision boundary)
X1_range <- seq(
  from = min(df_train$X1, df_test$X1),
  to = max(df_train$X1, df_test$X1),
  length.out = 150
)
X2_range <- seq(
  from = min(df_train$X2, df_test$X2),
  to = max(df_train$X2, df_test$X2),
  length.out = 150
)
grid <- expand.grid(X1 = X1_range, X2 = X2_range)

# Predict using the neural network for each point in the grid
grid_predictions <- predict(network_train, as.matrix(grid))

# Convert predictions to classes
grid_class <- data.frame(
  X1 = grid$X1,
  X2 = grid$X2,
  Y = factor(
    ifelse(grid_predictions > 0.5, 1, 0),
    levels = c(0, 1)
  )
)

# Visualize data
net_train_plot <- ggplot() +
  geom_point(
    data = grid_class, aes(x = X1, y = X2, color = Y),
    size = 1, shape = 21, alpha = 0.5
  ) +
  geom_point(
    data = df_train, aes(x = X1, y = X2, color = Y),
    size = 2.5
  ) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(
    title = "Neural Network Decision Boundary",
    subtitle = "Training Data"
  ) +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ); net_train_plot

# Checking testing accuracy
network_prob_test <- predict(network_train, newdata = df_test)
network_class_test <- ifelse(network_prob_test > 0.50, 1, 0)

# Accuracy
mean(network_class_test == df_test$Y)

# Visualize data
net_test_plot <- ggplot() +
  geom_point(
    data = grid_class, aes(x = X1, y = X2, color = Y),
    size = 1, shape = 21, alpha = 0.5
  ) +
  geom_point(
    data = df_test, aes(x = X1, y = X2, color = Y),
    size = 2.5
  ) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(
    title = "Neural Network Decision Boundary",
    subtitle = "Testing Data"
  ) +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ); net_test_plot

# Combine plots
net_plot <- ggarrange(
  net_train_plot, net_test_plot,
  nrow = 1
); net_plot


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Perform relative importance ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# X1

## Create copy
df_copy <- df_train

## Initialize store
store_accuracy_delta <- numeric(10) # number of iterations

# Accuracy
train_accuracy <- mean(network_class_train == df_train$Y)

## Shuffle and get prediction values
for(i in seq_along(store_accuracy_delta)){

  # Replace "X1" in copy
  df_copy$X1 <- sample(df_train$X1)

  # Get class
  predicted_class <- ifelse(
    predict(network_train, newdata = df_copy) > 0.50, 1, 0
  )

  # Get accuracy
  store_accuracy_delta[i] <- train_accuracy - mean(predicted_class == df_train$Y)

}

# Get mean of differences
mean(store_accuracy_delta)

# X2

## Create copy
df_copy <- df_train

## Initialize store
store_accuracy_delta <- numeric(10) # number of iterations

## Shuffle and get prediction values
for(i in seq_along(store_accuracy_delta)){

  # Replace "X2" in copy
  df_copy$X2 <- sample(df_train$X2)

  # Get class
  predicted_class <- ifelse(
    predict(network_train, newdata = df_copy) > 0.50, 1, 0
  )

  # Get accuracy
  store_accuracy_delta[i] <- train_accuracy - mean(predicted_class == df_train$Y)

}

# Get mean of differences
mean(store_accuracy_delta)

# X2 appears to be nearly 2x as important

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Neural network with {keras} ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Let's set up an environment
library(reticulate)

# Use conda environment
use_condaenv(condaenv = "neural-nets")

# Load packages
library(keras); library(tensorflow)

# Check for GPU
tf$config$list_physical_devices("GPU")

# Set seed
tensorflow::set_random_seed(1234)

# Documentation for {tensorflow} in R
# https://tensorflow.rstudio.com/
# RTFM

# Set up model
model <- keras_model_sequential() %>%
  layer_dense( # input layer => hidden layer
    units = 6, # number of neurons in first hidden layer
    input_shape = 2, # number of predictors
    activation = "sigmoid" # activation function
  ) %>%
  layer_dense( # hidden layer => output layer
    units = 1, # one output
    activation = "sigmoid"
    # classification (2 classes) = "sigmoid"
    # multi-class (3 or more classes) = "softmax"
    # regression = "linear"
  )

# Set up backpropagation
model <- model %>% compile(
  loss = "binary_crossentropy", # set loss function
  optimizer = optimizer_nadam(
    learning_rate = 0.05, # learning rate (default = 0.001)
    beta_1 = 0.9, # decay on first moment
    beta_2 = 0.999 # decay on second moment
    # Do not change `beta_1` or `beta_2` without a
    # firm understanding of optimization:
    # https://tivadardanka.com/blog/the-mathematics-of-optimization-for-deep-learning

  ),
  # HML likes `optimizer_rmsprop`
  # I tend to prefer `optimizer_nadam`
  metrics = "accuracy" # evaluation metric
)

# Train the model
keras_training <- model %>%
  fit(
    x = as.matrix(df_train[,c("X1", "X2")]), # predictors
    y = as.numeric(df_train[,c("Y")]) - 1, # outcome
    epochs = 1000, # number of iterations
    batch_size = 8, # mini-batch
    validation_split = 0.20, # 80/20
    verbose = FALSE # whether to print progress
  )

# See final results
keras_training; plot(keras_training)

# Final training accuracy
keras_train_probs <- model %>% predict(as.matrix(df_train[,c("X1", "X2")]), type = "response")

# Convert to classes
keras_train_class <- ifelse(keras_train_probs > 0.5, 1, 0)

# Accuracy
mean(keras_train_class == df_train$Y)

# Create a grid (to visualize decision boundary)
X1_range <- seq(
  from = min(df_train$X1, df_test$X1),
  to = max(df_train$X1, df_test$X1),
  length.out = 150
)
X2_range <- seq(
  from = min(df_train$X2, df_test$X2),
  to = max(df_train$X2, df_test$X2),
  length.out = 150
)
grid <- expand.grid(X1 = X1_range, X2 = X2_range)

# Predict using the neural network for each point in the grid
grid_predictions <-  model %>% predict(as.matrix(grid), type = "response")

# Convert predictions to classes
grid_class <- data.frame(
  X1 = grid$X1,
  X2 = grid$X2,
  Y = factor(
    ifelse(grid_predictions > 0.5, 1, 0),
    levels = c(0, 1)
  )
)

# Visualize data
keras_train_plot <- ggplot() +
  geom_point(
    data = grid_class, aes(x = X1, y = X2, color = Y),
    size = 1, shape = 21, alpha = 0.5
  ) +
  geom_point(
    data = df_train, aes(x = X1, y = X2, color = Y),
    size = 2.5
  ) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(
    title = "{keras} Decision Boundary",
    subtitle = "Training Data"
  ) +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ); keras_train_plot

# Predict
keras_test_probs <- model %>% predict(as.matrix(df_test[,c("X1", "X2")]), type = "response")

# Convert to classes
keras_test_class <- ifelse(keras_test_probs > 0.5, 1, 0)

# Accuracy
mean(keras_test_class == df_test$Y)

# Visualize data
keras_test_plot <- ggplot() +
  geom_point(
    data = grid_class, aes(x = X1, y = X2, color = Y),
    size = 1, shape = 21, alpha = 0.5
  ) +
  geom_point(
    data = df_test, aes(x = X1, y = X2, color = Y),
    size = 2.5
  ) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(
    title = "{keras} Decision Boundary",
    subtitle = "Testing Data"
  ) +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  ); keras_test_plot

# Combine plots
keras_plot <- ggarrange(
  keras_train_plot, keras_test_plot,
  nrow = 1
); keras_plot

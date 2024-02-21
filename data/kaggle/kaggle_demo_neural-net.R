#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Kaggle Demo Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load training data
kaggle_train <- read.csv("./kaggle_train.csv")

# Check out columns
colnames(kaggle_train)

# You can remove "ID" from dataset
kaggle_train <- kaggle_train[,-1]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Model fitting section | start ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load packages
library(reticulate); library(keras); library(tensorflow)

# Use 'neural-nets' environment
use_condaenv("neural-nets")

# Set seed
tensorflow::set_random_seed(1234)

# Set up model
model <- keras_model_sequential() %>%
  layer_dense( # input layer => hidden layer
    units = 4, # number of neurons in first hidden layer
    input_shape = 17, # number of predictors
    activation = "sigmoid" # activation function
  ) %>%
  layer_activity_regularization(l2 = 0.1) %>%
  layer_dense( # input layer => hidden layer
    units = 4, # number of neurons in second hidden layer
    activation = "sigmoid" # activation function
  ) %>%
  # more hidden layers and/or normalization/regularization/dropout...
  layer_dense( # hidden layer => output layer
    units = 1, # number of output
    activation = "sigmoid"
    # classification (2 classes) = "sigmoid"
    # multi-class (3 or more classes) = "softmax"
    # regression = "linear"
  )

# Set up backpropagation
model <- model %>% compile(
  loss = "binary_crossentropy", # set loss function
  optimizer = optimizer_rmsprop(learning_rate = 0.001), # learning rate
  # optimizer_rmsprop(learning_rate = 0.001)
  metrics = "accuracy" # evaluation metric
)

# Train the model
keras_training <- model %>%
  fit(
    x = as.matrix(kaggle_train[,-18]), # predictors
    y = as.numeric(kaggle_train[,18]), # outcome
    epochs = 150, # number of iterations
    batch_size = 16, # mini-batch
    validation_split = 0.20 # 80/20
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Model fitting section | end ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load testing data
kaggle_test <- read.csv("./kaggle_test.csv")

# Check out columns
colnames(kaggle_test)
# Notice that "TARGET" is not there!
# That's what you are trying to predict!

# KEEP THE "ID" VARIABLE!
ID <- kaggle_test$ID

# OK to remove "ID" variable
kaggle_test <- kaggle_test[,-1]

# Get predictions
test_prediction <- model %>%
  predict(as.matrix(kaggle_test))

# Get classifications
test_classes <- ifelse(
  test_prediction > 0.50, 1, 0
)

# Accuracy = 0.6296

# Set up .csv to turn into Kaggle
kaggle_result <- data.frame(
  ID = ID,
  TARGET = test_classes
)

# Save your .csv
write.csv(
  kaggle_result,
  file = "Alex_Christensen_logistic.csv",
  row.names = FALSE
)

# Submit to Kaggle:
# https://www.kaggle.com/t/1c68593599ad49f38fe5d3820aa2e278


# Submitting this logistic regression earns you 1 point!

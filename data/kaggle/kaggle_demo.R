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

# Logistic regression (baseline for 1 point)
kaggle_model <- glm(
  TARGET ~ ., data = kaggle_train,
  family = "binomial"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Model fitting section | end ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Check out summary of the model
summary(kaggle_model)

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
test_prediction <- predict(
  kaggle_model, newdata = kaggle_test,
  type = "response"
)

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

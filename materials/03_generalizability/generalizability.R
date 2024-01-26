#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PSY-GS 8875 | Week 3: Generalizability ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%
## Basics ----
#%%%%%%%%%%%%%

# Install packages
install.packages("caret")
# Getting an error when installing?
# If R says a package is "missing" or "unavailable",
# then install that package first

# R packages are linked in many different and intricate ways
# Installing should be straightforward but sometimes it's not
# Read carefully the errors that you get when installing
# When in doubt, restart R/RStudio:
# 1. Click on the "Session" drop-down menu
# 2. Click on "Restart R"
# 3. Try installing the package(s) again

# Load packages
library(caret)

# Load data
math <- read.csv("../../data/student_math/student_math_clean.csv")

# Paths

# "../" = go up a directory from your working directory
# What's your working directory? Type `getwd()` into your console
# You can also click the "Files" tab in your RStudio pane to see current files

# "./" = file in working directory
# This usage isn't necessary but can be helpful notation

# Let's try reading the path to data file on my computer:
#
# "../../" = go up 2 directories
# "../../data" = go up 2 directories and then into the "data" folder
# All together = go up 2 directories, into the "data" folder,
# into the "student_math" folder, and read in the
# data file called "student_math_clean.csv"

# You can also type into your console:
# math <- read.csv(file.choose())
# This code will open a graphical user interface for you to select a .csv file

# Once the data are loaded, you can check out the data
str(math)
# int = integer (stored as non-decimal number)
# num = double  (stored as decimal number)
# chr = character
#

# Select variables
math_voi <- math[,c(
  "final_grade", "study_time", "class_failures",
  "school_support", "family_support", "extra_paid_classes",
  "higher_ed", "internet_access", "absences"
)]

# Subsetting
# `c()` = concatenate or combine
#
# `[row, column]` = the brackets `[` and `]` say in the
# matrix of the data get the elements based on the `row`
# and `column`
# Leaving `row` or `column` blank will mean get *all* values
#
# `math[1,]` = get all values in row one (rows are usually single observations)
# `math[,1]` = get all values in column one (columns are usually single variables)
#
# `math[c(1:10),]` = get all values in rows 1-10
# `math[,c(1:10)]` = get all values in columns 1-10
#
# You can also use variable names (like above) to extract variables
# Reading the above code, we say:
# Get the variables "final_grade", "study_time", "class_failures",
# "school_support", "family_support", "extra_paid_classes",
# "higher_ed", "internet_access", "absences"
# and put them into the object `math_voi`

# Check out what we've done:
str(math_voi)

# Notice that we have variables that are `chr`
# Character variables will not be able to be used in analyses,
# so we need to somehow change these character to numeric values

# Set study time
replace_values <- 1:4
names(replace_values) <- unique(math_voi$study_time)
math_voi$study_time <- replace_values[math_voi$study_time]

# Rather than me telling you what's happening here, try
# having ChatGPT explain it to you
#
# Copy and paste the following into ChatGPT:
#
# Please tell me what's happening in the following code.
# I first provide the structure of the data and then
# the code that I'd like explained to me
#
# > str(math_voi)
# 'data.frame':	395 obs. of  9 variables:
#   $ final_grade       : int  6 6 10 15 10 15 11 6 19 15 ...
# $ study_time        : chr  "2 to 5 hours" "2 to 5 hours" "2 to 5 hours" "5 to 10 hours" ...
# $ class_failures    : int  0 0 3 0 0 0 0 0 0 0 ...
# $ school_support    : chr  "yes" "no" "yes" "no" ...
# $ family_support    : chr  "no" "yes" "no" "yes" ...
# $ extra_paid_classes: chr  "no" "no" "yes" "yes" ...
# $ higher_ed         : chr  "yes" "yes" "yes" "yes" ...
# $ internet_access   : chr  "no" "yes" "yes" "yes" ...
# $ absences          : int  6 4 10 2 4 10 0 6 0 0 ...
#
# # Set study time
# replace_values <- 1:4
# names(replace_values) <- unique(math_voi$study_time)
# math_voi$study_time <- replace_values[math_voi$study_time]

# For "yes/no" or "TRUE/FALSE" responses, we can convert them
# to binary variables

# Set "yes/no" responses to numeric
math_voi[,4:8] <- ifelse(math_voi[,4:8] == "yes", 1, 0)

# `ifelse` is a nice function that says:
#
# ifelse(condition, condition = TRUE, condition = FALSE)
#
# Reading from our `ifelse` above, it says:
#
# condition = columns 4-8 *equal* (double equal signs) "yes"
#
# So, we are setting the condition to be which values
# in all rows and in columns 4, 5, 6, 7, and 8 that are
# equal to "yes", then TRUE; otherwise, FALSE
#
# OK, so now we evaluate our condition = TRUE and condition = FALSE
#
# For values that are TRUE (or equal "yes"), then give them the value `1`
# For values that are FALSE (or equal "no"), then give them the value `0`

# Notice how our values have changed
str(math_voi)
# We now have all numeric values (int and num) !
# We can now run analyses on these data

# Linear regression
math_lm <- lm(
  final_grade ~ .,
  data = math_voi
)

# Let's breakdown this function:
#
# `lm` = linear model (type `?lm` for help/documentation)
#
# `final_grade ~ .`
#
# Ok.. there is a lot going on here..
#
# `final_grade` = outcome variable
# `~` = predicted by (this character is called tilde [til-duh])
# `.` = use all other variables in the data as predictors
#
# You can also do this:
math_lm <- lm(
  final_grade ~ study_time + class_failures + school_support +
  family_support + extra_paid_classes + higher_ed +
  internet_access + absences,
  data = math_voi
)
# This form is *equivalent* to `final_grade ~ .`
# This form is expressions more verbosely that we would like
# to use these variables to predict `final_grade`

# Check out the summary of our model
summary(math_lm)
# Estimate = beta coefficients

# Nice, now let's get *predictions* from our linear model
predict_grade <- predict(math_lm, newdata = math_voi)

# `predict` = get predictions
# `newdata` = which data to predict new values for outcome variable from
# Your data input into `newdata` *must* have the same variable names
# as the data used in your model (double and triple check)

# Compute RMSE
sqrt(mean((predict_grade - math_voi$final_grade)^2))
# sqrt( # (square) root
#   mean( # mean
#     (
#       predict_grade - math_voi$final_grade # error
#     )^2 # square
#   )
# )

# Awesome. OK, how about logistic regression?

# Logistic regression
math_log <- glm(
  extra_paid_classes ~ .,
  data = math_voi,
  family = "binomial"
  # `family` = distribution or link to use in regression
  # "binomial" = logit or logistic link (i.e., logistic regression)
)

# Check out the summary of our model
summary(math_log)
# Estimate = log of the odds-ratio

# We often like to interpret *odds-ratios* rather than
# log of the odds-ratio

# Get odds-ratios
exp(coef(math_log))
# `coef` = get coefficients of the regression model
# `exp` = take the exponential (or e) of the coefficients
# Result: odds-ratios

# With logistic regression, we want to compute
# accuracy rather than RMSE
predict_paid_classes <- predict(
  math_log, newdata = math_voi,
  type = "response" # sets prediction to get probabilities
)

# We have probabilities that our data belong to either
# 1 (or "yes") extra paid classes or 0 (or "no") extra paid classes
paid_classes <- ifelse(
  predict_paid_classes > 0.50, 1, 0
)
# Converts probabilities greater than 0.50 to
# 1 (paid classes) and less than or equal to 0.50
# to 0 (no paid classes)

# Now, we can see how accuracy we were
mean(paid_classes == math_voi$extra_paid_classes)
# We correctly predicted 67.3% of cases

#%%%%%%%%%%%%%%%%%%%%%
## Data splitting ----
#%%%%%%%%%%%%%%%%%%%%%

# Set seed for reproducibility
set.seed(1234) # don't forget!!

# Get training indices
training <- sample(
  1:nrow(math_voi), # sequence through the cases
  round(nrow(math_voi) * 0.80), # 0.70 or 0.80
  replace = FALSE # do not sample with replacement
)

# Split your data
math_train <- math_voi[training,]
math_test <- math_voi[-training,]

# Fit your model
fit <- lm(final_grade ~., data = math_train)
# replace `model` with your model

# Predict your test data
predict_grade <- predict(fit, newdata = math_test)
# Arguments can change depending on function used,
# so RTFM (read the f*cking manual: ?predict.model)

# Perform some evaluation
sqrt(mean((predict_grade - math_test$final)^2))
# e.g., root mean square error in linear regression

#%%%%%%%%%%%%%%%%%%%%
## Bootstrapping ----
#%%%%%%%%%%%%%%%%%%%%

# Set up training for {caret}
train_control <- trainControl(
  method = "boot", # training method
  number = 100 # number of bootstraps
)

# Perform bootstrap
math_boot <- train(
  form = final_grade ~ ., # formula input
  data = math_train, # data input
  method = "lm", # linear model
  metric = "RMSE", # default
  trControl = train_control # training parameters
)

# Descriptive statistics
mean(math_boot$resample$RMSE)
median(math_boot$resample$RMSE)
sd(math_boot$resample$RMSE)
range(math_boot$resample$RMSE)

# Compute relative importance
varImp(math_boot)$importance

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## k-folds Cross-validation ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Set up training for {caret}
train_control <- trainControl(
  method = "cv", # training method
  number = 5 # number of folds
)

# Perform bootstrap
math_cv <- train(
  form = final_grade ~ ., # formula input
  data = math_train, # data input
  method = "lm", # linear model
  metric = "RMSE", # default
  trControl = train_control # training parameters
)

# k-folds results
math_cv$resample

# Print summary
cat("Mean: ", round(mean(math_cv$resample$RMSE), 3))
cat("Median: ", round(median(math_cv$resample$RMSE), 3))
cat("SD: ", round(sd(math_cv$resample$RMSE), 3))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Leave-one-out Cross-validation ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Set up training for {caret}
train_control <- trainControl(
  method = "LOOCV" # training method
  # DO NOT USE:
  # `method = "cv", number = nrow(math_train)`
  # Though this setting is equivalent in THEORY,
  # it is not what happens in PRACTICE
  # {caret} will compute something different
)

# Perform bootstrap
math_cv <- train(
  form = final_grade ~ ., # formula input
  data = math_train, # data input
  method = "lm", # linear model
  metric = "RMSE", # default
  trControl = train_control # training parameters
)

# Leave-one-out results
math_cv$resample

# Print summary
cat("Mean: ", round(mean(math_cv$resample$RMSE), 3))
cat("Median: ", round(median(math_cv$resample$RMSE), 3))
cat("SD: ", round(sd(math_cv$resample$RMSE), 3))
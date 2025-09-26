### Day 1 - Part 2

library(tidyverse)
library(tidymodels)


# Red Wine Data Prep ----------------------------------------------------------------

red_wine <- read_csv(file = "Input Data/red_wine.csv") # Load the dataset

red_wine <- red_wine %>%
  drop_na() %>%
  select(-wine_id) %>%
  mutate(quality = as.numeric(quality))


# Red Wine Data Splitt ----------------------------------------------------------------

set.seed(42) # Set seed to easily reproduce results

red_wine_split <- red_wine %>%
  initial_split(prop = 0.80) # Specify proportion of data to allocate to train

train_data <- training(red_wine_split)

test_data <- testing(red_wine_split)

summary(train_data)
summary(test_data)


# Inspecting the split datasets -------------------------------------------

View(train_data)
View(test_data)

# Target variable distribution
train_data %>%
  ggplot(aes(x = quality)) +
  geom_bar() +
  ggtitle("Train dataset")

test_data %>%
  ggplot(aes(x = quality)) +
  geom_bar() +
  ggtitle("Test dataset")


# Specifing model type and computational engine -----------------------------

lm_model <-
  linear_reg() %>% # Model type: Linear Regression
  set_engine("lm") # Computational engine: lm

# glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models
# https://glmnet.stanford.edu/articles/glmnet.html

glmnet_model <-
  linear_reg(penalty = 1) %>% # Model type: Linear Regression
  set_engine("glmnet") # Computational engine: glmnet

# penalty: Amount of Regularization (type: double, default: see below)

# mixture: Proportion of Lasso Penalty (type: double, default: 1.0)

# A value of mixture = 1 corresponds to a pure lasso model, 
# while mixture = 0 indicates ridge regression.

# The penalty parameter has no default and requires a single numeric value.


# Fit models --------------------------------------------------------------

lm_fit <-
  lm_model %>%
  fit(quality ~ ., data = train_data)

glmnet_fit <-
  glmnet_model %>%
  fit(quality ~ ., data = train_data)


# Inspect model fits -------------------------------------------------------

lm_fit$fit %>% coef()
lm_fit$fit %>% summary()

#glmnet_fit$fit %>% coef()
#glmnet_fit$fit %>% summary()


# Generate predictions ----------------------------------------------------

lm_pred <- predict(lm_fit, new_data = test_data)

glmnet_pred <- predict(glmnet_fit, new_data = test_data)

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>%
  select(quality) %>% # keep target variable (also known as the truth)
  mutate(lm_pred = round(lm_pred$.pred, 0), # lm model predictions
         glmnet_pred = round(glmnet_pred$.pred, 0)) # glmnet model predictions

View(predictions)


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = quality, estimate = lm_pred)

metrics(predictions, truth = quality, estimate = glmnet_pred)


# Performance Metric that measures overall accuracy achieved --------------
# (the mean percentage of accurate wine quality predictions)

# Add columns to indicate if the predictions were correct
predictions$truth_lm <- predictions$quality == predictions$lm_pred
predictions$truth_glmnet <- predictions$quality == predictions$glmnet_pred

# Calculate overall accuracy for each model
mean(predictions$quality == predictions$lm_pred)

mean(predictions$quality == predictions$glmnet_pred)

# White Wine Data Prep ----------------------------------------------------------------

white_wine <- read_csv(file = "Input Data/white_wine.csv") # Load the dataset

white_wine <- white_wine %>%
  drop_na() %>%
  select(-wine_id) %>%
  filter(quality >= 0) %>%
  filter(ph <= 14 & ph >= 0) %>%
  mutate(quality = as.numeric(quality))


# White Wine Data Splitt ----------------------------------------------------------------

set.seed(42) # Set seed to easily reproduce results

white_wine_split <- white_wine %>%
  initial_split(prop = 0.80) # Specify proportion of data to allocate to train

train_data <- training(white_wine_split)

test_data <- testing(white_wine_split)

summary(train_data)
summary(test_data)


# Inspecting the split datasets -------------------------------------------

View(train_data)
View(test_data)

# Target variable distribution
train_data %>%
  ggplot(aes(x = quality)) +
  geom_bar() +
  ggtitle("Train dataset")

test_data %>%
  ggplot(aes(x = quality)) +
  geom_bar() +
  ggtitle("Test dataset")


# Specifing model type and computational engine -----------------------------

lm_model <-
  linear_reg() %>% # Model type: Linear Regression
  set_engine("lm") # Computational engine: lm

# glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models
# https://glmnet.stanford.edu/articles/glmnet.html

glmnet_model <-
  linear_reg(penalty = 1) %>% # Model type: Linear Regression
  set_engine("glmnet") # Computational engine: glmnet

# penalty: Amount of Regularization (type: double, default: see below)

# mixture: Proportion of Lasso Penalty (type: double, default: 1.0)

# A value of mixture = 1 corresponds to a pure lasso model, 
# while mixture = 0 indicates ridge regression.

# The penalty parameter has no default and requires a single numeric value.


# Fit models --------------------------------------------------------------

lm_fit <-
  lm_model %>%
  fit(quality ~ ., data = train_data)

glmnet_fit <-
  glmnet_model %>%
  fit(quality ~ ., data = train_data)


# Inspect model fits -------------------------------------------------------

lm_fit$fit %>% coef()
lm_fit$fit %>% summary()

#glmnet_fit$fit %>% coef()
#glmnet_fit$fit %>% summary()

# Generate predictions ----------------------------------------------------

lm_pred <- predict(lm_fit, new_data = test_data)

glmnet_pred <- predict(glmnet_fit, new_data = test_data)

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>%
  select(quality) %>% # keep target variable (also known as the truth)
  mutate(lm_pred = round(lm_pred$.pred, 0), # lm model predictions
         glmnet_pred = round(glmnet_pred$.pred, 0)) # glmnet model predictions

View(predictions)


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = quality, estimate = lm_pred)

metrics(predictions, truth = quality, estimate = glmnet_pred)



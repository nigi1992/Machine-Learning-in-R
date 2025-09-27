### Day 2 - Part 1

# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)

#install.packages("randomForest")
#install.packages("ranger")

library(rpart)
library(randomForest)
library(ranger)


# Load Data ---------------------------------------------------------------

white_wine <- read_csv(file = "Input Data/white_wine.csv")

# Prepare data ------------------------------------------------------------

white_wine <- white_wine %>% 
  drop_na() %>% 
  filter(quality >= 0) %>%
  filter(ph <= 14 & ph >= 0) %>% 
  select(-wine_id) %>% 
  mutate(quality = as.factor(quality))


# Split the dataset -------------------------------------------------------

set.seed(42) # Set seed to easily reproduce results

white_wine_split <- white_wine %>% 
  initial_split(prop = 0.80, strata = quality)

train_data <- training(white_wine_split)

test_data <- testing(white_wine_split)


# Specify model type and computational engine -----------------------------

#tree_model <- 
  #decision_tree() %>% # Model type: Decision Tree 
  #set_engine("rpart") %>% # Computational engine: rpart
  #set_mode("classification") %>%  # Specify model mode
  #set_args(min_n = 2, tree_depth = 5) # Specify model arguments

tree_model <-
  rand_forest() %>% # Model type: Random Forest
  set_engine("randomForest") %>% # Computational engine: randomForest
  set_mode("classification") %>%  # Specify model mode
  set_args(mtry = 3, trees = 500, min_n = 5) # Specify model arguments

# Fit models --------------------------------------------------------------

#tree_fit <- 
 # tree_model %>% 
  #fit(quality ~ ., data = train_data)

# Model fit with specified features using formula notation
tree_fit <-
  tree_model %>%
  fit(quality ~ volatile_acidity + residual_sugar + free_sulfur_dioxide + density + ph + sulphates + alcohol, data = train_data)


# Generate predictions ----------------------------------------------------

tree_pred <- predict(tree_fit, new_data = test_data, type = "class")

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>% 
  select(quality) %>% # keep target variable (also known as the truth)
  mutate(tree_pred = tree_pred$.pred_class) # glmnet model predictions


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = quality, estimate = tree_pred)

conf_mat(predictions, truth = quality, estimate = tree_pred)
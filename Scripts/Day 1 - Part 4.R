### Day 1 - Part 4

# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)

# Load Read Wine Data ---------------------------------------------------------------

red_wine <- read_csv(file = "Input Data/red_wine.csv")

# Prepare data ------------------------------------------------------------

red_wine <- red_wine %>%
  drop_na() %>%
  select(-wine_id) %>%
  mutate(quality = as.factor(quality))

# Split the dataset -------------------------------------------------------

set.seed(42) # Set seed to easily reproduce results

red_wine_split <- red_wine %>%
  initial_split(prop = 0.80, strata = quality)

train_data <- training(red_wine_split)

test_data <- testing(red_wine_split)

# Specify model type and computational engine -----------------------------

tree_model <-
  decision_tree() %>% # Model type: Decision Tree
  set_engine("rpart") %>% # Computational engine: rpart
  set_mode("classification") %>% # Specify model mode
  set_args(min_n = 1, tree_depth = 10) # Specify model arguments

# Fit models --------------------------------------------------------------

# Model fit with all features (implied by ".") using formula notation
#fit(quality ~ ., data = train_data)

# Model fit with specified features using formula notation
#fit(quality ~ volatile_acidity + citric_acid + alcohol, data = train_data)

# Model fit with specified features using (x,y) arguments
#fit_xy(x = train_data[, c("volatile_acidity", "citric_acid", "alcohol")],
 #      y = train_data$quality)

tree_fit <-
  tree_model %>%
  fit(quality ~ ., data = train_data)

# Generate predictions ----------------------------------------------------

tree_pred <- predict(tree_fit, new_data = test_data, type = "class")

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>%
  select(quality) %>% # keep target variable (also known as the truth)
  mutate(tree_pred = tree_pred$.pred_class) # glmnet model predictions


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = quality, estimate = tree_pred)

conf_mat(predictions, truth = quality, estimate = tree_pred)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)

# Load White Wine Data ---------------------------------------------------------------

white_wine <- read_csv(file = "Input Data/white_wine.csv")

# Prepare data ------------------------------------------------------------

white_wine <- white_wine %>%
  drop_na() %>%
  select(-wine_id) %>%
  filter(quality >= 0) %>%
  filter(ph <= 14 & ph >= 0) %>%
  mutate(quality = as.factor(quality))

# Split the dataset -------------------------------------------------------

set.seed(42) # Set seed to easily reproduce results

white_wine_split <- white_wine %>%
  initial_split(prop = 0.80, strata = quality)

train_data <- training(white_wine_split)

test_data <- testing(white_wine_split)

# Specify model type and computational engine -----------------------------

tree_model <-
  decision_tree() %>% # Model type: Decision Tree
  set_engine("rpart") %>% # Computational engine: rpart
  set_mode("classification") %>% # Specify model mode
  set_args(min_n = 1, tree_depth = 10) # Specify model arguments

# Fit models --------------------------------------------------------------

# Model fit with all features (implied by ".") using formula notation
#fit(quality ~ ., data = train_data)

# Model fit with specified features using formula notation
#fit(quality ~ volatile_acidity + citric_acid + alcohol, data = train_data)

# Model fit with specified features using (x,y) arguments
#fit_xy(x = train_data[, c("volatile_acidity", "citric_acid", "alcohol")],
#      y = train_data$quality)

tree_fit <-
  tree_model %>%
  fit(quality ~ ., data = train_data)

# Generate predictions ----------------------------------------------------

tree_pred <- predict(tree_fit, new_data = test_data, type = "class")

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>%
  select(quality) %>% # keep target variable (also known as the truth)
  mutate(tree_pred = tree_pred$.pred_class) # glmnet model predictions


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = quality, estimate = tree_pred)

conf_mat(predictions, truth = quality, estimate = tree_pred)


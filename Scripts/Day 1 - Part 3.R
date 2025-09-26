### Day 1 - Part 3

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

multinom_model <-
  multinom_reg(penalty = 0.005) %>% # Model type: Multinomial Regression
  set_engine("glmnet") # Computational engine: glmnet

# Fit models --------------------------------------------------------------

multinom_fit <-
  multinom_model %>%
  fit(quality ~ ., data = train_data)

# Generate predictions ----------------------------------------------------

multinom_pred <- predict(multinom_fit, new_data = test_data, penalty = 0.005)

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>%
  select(quality) %>% # keep target variable (also known as the truth)
  mutate(multinom_pred = multinom_pred$.pred_class) # glmnet model predictions

View(predictions)

# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = quality, estimate = multinom_pred)


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

multinom_model <-
  multinom_reg(penalty = 0.005) %>% # Model type: Multinomial Regression
  set_engine("glmnet") # Computational engine: glmnet

# Fit models --------------------------------------------------------------

multinom_fit <-
  multinom_model %>%
  fit(quality ~ ., data = train_data)

# Generate predictions ----------------------------------------------------

multinom_pred <- predict(multinom_fit, new_data = test_data, penalty = 0.005)

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>%
  select(quality) %>% # keep target variable (also known as the truth)
  mutate(multinom_pred = multinom_pred$.pred_class) # glmnet model predictions

View(predictions)


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = quality, estimate = multinom_pred)


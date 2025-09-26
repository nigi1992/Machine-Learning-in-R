
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
### Day 1 - Part 4

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
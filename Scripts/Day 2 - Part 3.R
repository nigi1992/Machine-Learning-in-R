### Day 2 - Part 3

library(tidyverse)
library(tidymodels)

# Load Data Set Churn

churn  <- read_csv(file = "Input Data/churn.csv")
#view(churn)

is.factor(churn$churned)

churn <- churn %>%
  #drop_na() %>%
  mutate(churned = as.factor(churned)) %>%
  select(-customer_id) %>%
  select(-surname)

is.factor(churn$churned)


# Splitting the dataset ---------------------------------------------------

set.seed(42) # Set seed to easily reproduce results

churn_split <- churn %>%
  #select(-geography, -gender) %>% 
  initial_split(prop = 0.80, strata = churned)

train_data <- training(churn_split)

test_data <- testing(churn_split)

summary(train_data)

summary(test_data)


# Pre-Processing ----------------------------------------------------------
# Recipe ------------------------------------------------------------------

df_rec <- recipe(churned ~ ., data = train_data)

df_rec <- df_rec %>%
  step_normalize(credit_score, age, tenure, balance, 
                 num_of_products, estimated_salary) %>%
  step_dummy(geography, gender, one_hot = TRUE)


# Prepare -----------------------------------------------------------------

df_prep <- prep(df_rec, training = train_data)


# Bake --------------------------------------------------------------------

test_data_baked <- bake(df_prep, new_data = test_data)
summary(test_data_baked)
train_data_baked <- bake(df_prep, new_data = train_data)
summary(train_data_baked)
# Note: Centres of the test set are not 0 because the normalising process
# uses the train data... the test dataset should never be used for these
# operations


# Random Forrest Ranger Engine ----------------------------------------------

tree_model <-
  rand_forest() %>% # Model type: Random Forrest
  set_engine("ranger") %>% # Computational engine: 
  set_mode("classification") %>% # Specify model mode
  set_args(min_n = 1, tree_depth = 10) # Specify model arguments

# Fit models --------------------------------------------------------------

tree_fit <-
  tree_model %>%
  fit(churned ~ ., data = train_data_baked)


# Generate predictions ----------------------------------------------------

tree_pred_class <- predict(tree_fit, new_data = test_data_baked, type = "class")

tree_pred_prob <- predict(tree_fit, new_data = test_data_baked, type = "prob")


# Data frame from test set with “attached” the model predictions
predictions <- test_data_baked %>%
  select(churned) %>% # keep target variable (also known as the truth)
  bind_cols(., tree_pred_prob, tree_pred_class)

two_class_curve <- roc_curve(predictions, churned, .pred_1, event_level = "second")
# event_level = "second" is needed because "churned" is a 2 level factor
# and the level we are interested is when "churned = 1" which is the
# second level

autoplot(two_class_curve)

roc_auc(predictions, churned, .pred_1, event_level = "second")



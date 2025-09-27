### Day 2 - Part 2

library(tidyverse)
library(tidymodels)

# Load Data Set Churn

churn  <- read_csv(file = "Input Data/churn.csv")
#view(churn)

# Show NA's in the dataset
colSums(is.na(churn))

is.factor(churn$churned)

churn <- churn %>%
  #drop_na() %>%
  mutate(churned = as.factor(churned)) %>%
  select(-customer_id) #%>%
  #select(-gender) %>%
  #select(-geography) %>%
  #select(-surname)

is.factor(churn$churned)

glimpse(churn)
summary(churn)
str(churn)

table(churn$gender)
table(churn$geography)

#table(churn$surname)
# rank surname by frequency
table(churn$surname) %>%
  sort(decreasing = TRUE) %>%
  head(20)

# Target variable distribution
churn %>%
  ggplot(aes(x = gender)) +
  geom_bar() +
  ggtitle("Gender Distribution")

churn %>%
  ggplot(aes(x = geography)) +
  geom_bar() +
  ggtitle("Geography Distribution")


# Splitting the dataset ---------------------------------------------------

churn <- churn %>%
  select(-gender) %>%
  select(-geography) %>%
  select(-surname)

glimpse(churn)
summary(churn)
str(churn)


set.seed(42) # Set seed to easily reproduce results

churn_split <- churn %>%
  initial_split(prop = 0.80, strata = churned)

train_data <- training(churn_split)

test_data <- testing(churn_split)

summary(train_data)

summary(test_data)

# Target variable distribution
train_data %>%
  ggplot(aes(x = churned)) +
  geom_bar() +
  ggtitle("Train dataset")

test_data %>%
  ggplot(aes(x = churned)) +
  geom_bar() +
  ggtitle("Test dataset")

# Reshape data for plots
plot_data <- churn %>%
  pivot_longer(cols = -churned, names_to = "key", values_to = "value")

# Feature Distribution plots
plot_data %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ key, scales = "free")

# Feature Distribution plots by churned
plot_data %>%
  ggplot(aes(x = value, fill = churned)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ key, scales = "free")

# Feature Box plots
plot_data %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

library(corrplot)
library(rpart.plot)

# Correlation plot (target is class)
churn %>%  
  select(-churned) %>%
  cor() %>%
  corrplot.mixed(order = "hclust",
                 upper = "circle",
                 lower = "number",
                 tl.pos = 'lt',
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)

# Correlation plot (target is numeric) churned
churn %>%  
  mutate(churned = as.numeric(churned)) %>%
  cor() %>%
  corrplot.mixed(upper = "circle",
                 lower = "number",
                 tl.pos = 'lt',
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)

# Specifing model type and computational engine -----------------------------

tree_model <-
  decision_tree() %>% # Model type: Decision Tree
  set_engine("rpart") %>% # Computational engine: rpart
  set_mode("classification") %>% # Specify model mode
  set_args(min_n = 2, tree_depth = 5) # Specify model arguments


# Fit models --------------------------------------------------------------

is.factor(train_data$churned)

tree_fit <-
  tree_model %>%
  fit(churned ~ ., data = train_data)


# Generate predictions ----------------------------------------------------

is.factor(test_data$churned)

tree_pred <- predict(tree_fit, new_data = test_data, type = "class")

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>% 
  select(churned) %>% # keep target variable (also known as the truth)
  mutate(tree_pred = tree_pred$.pred_class) # glmnet model predictions


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = churned, estimate = tree_pred)

conf_mat(predictions, truth = churned, estimate = tree_pred)


# Visualize the decision tree ----------------------------------------------

library(rpart.plot)
rpart.plot(tree_fit$fit, type = 3, extra = 101)


# Random Forrest -----------------------------

tree_model <-
  rand_forest() %>% # Model type: Random Forrest
  set_engine("randomForest") %>% # Computational engine: 
  set_mode("classification") %>% # Specify model mode
  set_args(min_n = 2, tree_depth = 5) # Specify model arguments


# Fit models --------------------------------------------------------------

tree_fit <-
  tree_model %>%
  fit(churned ~ ., data = train_data)


# Generate predictions ----------------------------------------------------

tree_pred <- predict(tree_fit, new_data = test_data, type = "class")

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>% 
  select(churned) %>% # keep target variable (also known as the truth)
  mutate(tree_pred = tree_pred$.pred_class) # glmnet model predictions


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = churned, estimate = tree_pred)

conf_mat(predictions, truth = churned, estimate = tree_pred)


# Random Forrest Ranger Engine ----------------------------------------------

tree_model <-
  rand_forest() %>% # Model type: Random Forrest
  set_engine("ranger") %>% # Computational engine: 
  set_mode("classification")#%>% # Specify model mode
  #set_args(min_n = 1, tree_depth = 10) # Specify model arguments

# Fit models --------------------------------------------------------------

tree_fit <-
  tree_model %>%
  fit(churned ~ ., data = train_data)


# Generate predictions ----------------------------------------------------

tree_pred <- predict(tree_fit, new_data = test_data, type = "class")

# Data frame from test set with “attached” the model predictions
predictions <- test_data %>% 
  select(churned) %>% # keep target variable (also known as the truth)
  mutate(tree_pred = tree_pred$.pred_class) # glmnet model predictions


# Calculate performance metrics -------------------------------------------

metrics(predictions, truth = churned, estimate = tree_pred)

conf_mat(predictions, truth = churned, estimate = tree_pred)


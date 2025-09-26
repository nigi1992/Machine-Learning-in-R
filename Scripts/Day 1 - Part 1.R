### Day 1 - Part 1

library(tidyverse)
library(tidymodels)

# Red Wine Data Prep ----------------------------------------------------------------

# read in data
red_wine <- read_csv(file = "Input Data/red_wine.csv") # Load the dataset

View(red_wine) # View the dataset
glimpse(red_wine) # Get a glimpse of the dataset
head(red_wine) # View the first few rows of the dataset
summary(red_wine) # Get a summary of the dataset

### Preliminary Data Exploration (EDA)

# Remove missing data
red_wine <- red_wine %>%
  drop_na()

# Remove identifier column
red_wine <- red_wine %>%
  select(-wine_id)

# Convert target variable to class
red_wine <- red_wine %>%
  mutate(quality = as.factor(quality))


# Red Wine Plots ----------------------------------------------------------

# Target variable distribution
red_wine %>%
  ggplot(aes(x = quality, fill = quality)) +
  geom_bar()

# Reshape data for plots
plot_data <- red_wine %>%
  pivot_longer(cols = -quality, names_to = "key", values_to = "value")

# Feature Distribution plots
plot_data %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ key, scales = "free")

# Feature Distribution plots by quality
plot_data %>%
  ggplot(aes(x = value, fill = quality)) +
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

# Feature Box plots by quality
plot_data %>%
  ggplot(aes(y = value, fill = quality)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ key, scales = "free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Correlation plot (target is class) red_wine %>%
red_wine %>%
    select(-quality) %>%
    cor() %>%
    corrplot.mixed(order = "hclust",
                   upper = "circle",
                   lower = "number",
                   tl.pos = 'lt',
                   tl.col = "black",
                   lower.col = "black",
                   number.cex = 1)
  
# Correlation plot (target is numeric) red_wine %>%
red_wine %>%
  mutate(quality = as.numeric(quality)) %>%
  cor() %>%
  corrplot.mixed(upper = "circle",
                 lower = "number",
                 tl.pos =
                   'lt'
                 ,
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)

# White Wine Data Prep --------------------------------------------------------------

# read in data
white_wine <- read_csv(file = "Input Data/white_wine.csv") # Load the dataset

View(white_wine) # View the dataset
glimpse(white_wine) # Get a glimpse of the dataset
head(white_wine) # View the first few rows of the dataset
summary(white_wine) # Get a summary of the dataset

### Preliminary Data Exploration (EDA)

# Remove missing data
white_wine <- white_wine %>%
  drop_na()

# Show missing values
colSums(is.na(white_wine))

# Remove identifier column
white_wine <- white_wine %>%
  select(-wine_id)

# Show negative values (if any) for quality column
white_wine %>%
  filter(quality < 0)

# Remove negative values (if any) for quality column
white_wine <- white_wine %>%
  filter(quality >= 0)

# Convert target variable to class
white_wine <- white_wine %>%
  mutate(quality = as.factor(quality))

is.factor(white_wine$quality)

# Show values in ph variable higher than 14 or lower than 0
white_wine %>%
  filter(ph > 14 | ph < 0)

# Remove values higher than 14 or lower than 0
white_wine <- white_wine %>%
  filter(ph <= 14 & ph >= 0)


# White Wine Plots --------------------------------------------------------

# Target variable distribution
white_wine %>%
  ggplot(aes(x = quality, fill = quality)) +
  geom_bar()  

# Reshape data for plots
plot_data <- white_wine %>%
  pivot_longer(cols = -quality, names_to = "key", values_to = "value")

# Feature Distribution plots
plot_data %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ key, scales = "free")

# Feature Distribution plots by quality
plot_data %>%
  ggplot(aes(x = value, fill = quality)) +
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

# Correlation plot (target is class) red_wine %>%
white_wine %>%  
  select(-quality) %>%
  cor() %>%
  corrplot.mixed(order = "hclust",
                 upper = "circle",
                 lower = "number",
                 tl.pos = 'lt',
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)

# Correlation plot (target is numeric) red_wine %>%
white_wine %>%  
  mutate(quality = as.numeric(quality)) %>%
  cor() %>%
  corrplot.mixed(upper = "circle",
                 lower = "number",
                 tl.pos = 'lt',
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)
# End of Day 1 - Part 1
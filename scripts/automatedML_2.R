# Load data
library(tidyverse)
library(readxl)

employee_attrition_tbl <- read_csv("scripts/data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("scripts/data/data_definitions.xlsx", sheet = 1, col_names = FALSE)

employee_attrition_tbl %>% 
  ggplot(aes(Education)) +
  geom_bar()

# Data preparation ----
# Human readable

definitions_tbl <- definitions_raw_tbl %>% 
  fill(...1, .direction = "down") %>%
  filter(!is.na(...2)) %>%
  separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
  rename(column_name = ...1) %>%
  mutate(key = as.numeric(key)) %>%
  mutate(value = value %>% str_replace(pattern = "'", replacement = "")) 
definitions_tbl

# DATA PREPARATION ----
# Human readable ----

definitions_list <- definitions_tbl %>% 
  
  # Mapping over lists
  
  # Split into multiple tibbles
  split(.$column_name) %>%
  # Remove column_name
  map(~ select(., -column_name)) %>%
  # Convert to factors because they are ordered an we want to maintain that order
  map(~ mutate(., value = as_factor(value))) 

# definitions_list[[1]]
definitions_list[["Education"]]

# Rename columns
for (i in seq_along(definitions_list)) {
  list_name <- names(definitions_list)[i]
  colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
}

definitions_list[["Education"]]


data_merged_tbl <- list(HR_Data = employee_attrition_tbl) %>%
  
  # Join everything
  append(definitions_list, after = 1) %>%
  reduce(left_join) %>%
  
  # Remove unnecessary columns
  select(-one_of(names(definitions_list))) %>%
  
  # Format the "_value"
  set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
  
  # Resort
  select(sort(names(.))) 

# Return only unique values of BusinessTravel
data_merged_tbl %>% 
  distinct(BusinessTravel)

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  glimpse()

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  glimpse()

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  map(levels)

data_processed_tbl <- data_merged_tbl %>%        
  mutate_if(is.character, as.factor) %>%
  mutate(
    BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                    "Travel_Rarely", 
                                                    "Travel_Frequently"),
    MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                   "Married", 
                                                   "Divorced")
  )

data_processed_tbl %>% 
  select_if(is.factor) %>% 
  map(levels)

process_hr_data_readable <- function(data, definitions_tbl) {
  
  definitions_list <- definitions_tbl %>%
    fill(...1, .direction = "down") %>%
    filter(!is.na(...2)) %>%
    separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = ...1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map(~ select(., -column_name)) %>%
    map(~ mutate(., value = as_factor(value))) 
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", 
                              replacement = "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                      "Travel_Rarely", 
                                                      "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                     "Married", 
                                                     "Divorced")
    )
  
  return(data_merged_tbl)
  
}
process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl) %>% 
  glimpse()

# DATA PREPARATION ----
# Machine readable ----

# libraries
library(rsample)
library(recipes)

# Processing pipeline
# If we had our raw data already split into train and test data
train_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)
test_readable_tbl   <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)

# Plot Faceted Histgoram function
# To create a function and test it, we can assign our data temporarily to data
data <- train_readable_tbl 

plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = "#2dc6d6", color = "white", 
                            ncol = 5, scale = "free") {
  
  data_factored <- data %>%
    
    # Convert input to make the function fail safe 
    # (if other content might be provided)
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    
    # Data must be in long format to make facets
    pivot_longer(cols = everything(),
                 names_to = "key",
                 values_to = "value",
                 # set key = factor() to keep the order
                 names_transform = list(key = forcats::fct_inorder)) 
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scale = scale)
  
  return(g)
  
}

# Example calls
train_readable_tbl %>% plot_hist_facet()
train_readable_tbl %>% plot_hist_facet(fct_rev = T)

# Bring attrition to the top (alt.: select(Attrition, everything()))
train_readable_tbl %>% 
  relocate(Attrition) %>% 
  plot_hist_facet()

# Data Preprocessing With Recipes ----

# Plan: Correlation Analysis

# 1. Zero Variance Features ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors())

recipe_obj %>% 
  prep()


# 2. Transformations ---- (for skewed features)
library(PerformanceAnalytics)  # for skewness  

skewed_feature_names <- train_readable_tbl %>%
  select(where(is.numeric)) %>%
  map_df(skewness) %>%
  pivot_longer(cols = everything(),
               names_to = "key",
               values_to = "value",
               names_transform = list(key = forcats::fct_inorder)) %>%
  arrange(desc(value)) %>%
  
  # Let's set the cutoff value to 0.7 (beccause TrainingTimesLastYear does not seem to be that skewed)
  filter(value >= 0.7) %>%
  pull(key) %>%
  as.character()

train_readable_tbl %>%
  select(all_of(skewed_feature_names)) %>%
  plot_hist_facet()

!skewed_feature_names %in% c("JobLevel", "StockOptionLevel")

skewed_feature_names <- train_readable_tbl %>%
  select(where(is.numeric)) %>%
  map_df(skewness) %>%
  pivot_longer(cols = everything(),
               names_to = "key",
               values_to = "value",
               names_transform = list(key = forcats::fct_inorder)) %>%
  arrange(desc(value)) %>%
  filter(value >= 0.7) %>%
  filter(!key %in% c("JobLevel", "StockOptionLevel")) %>%
  pull(key) %>%
  as.character()

# We need to convert those columns to factors in the next step
factor_names <- c("JobLevel", "StockOptionLevel")

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor)

recipe_obj %>% 
  prep() %>% 
  bake(train_readable_tbl) %>% 
  select(skewed_feature_names) %>%
  plot_hist_facet() 
# 3. Center and scale

# Plot numeric data
train_readable_tbl %>% 
  select(where(is.numeric)) %>% 
  plot_hist_facet()


recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

# You can compare the means attribute before and after prepping the recipe
recipe_obj$steps[[4]] # before prep
prepared_recipe <- recipe_obj %>% prep()
prepared_recipe$steps[[4]]

prepared_recipe %>%
  bake(new_data = train_readable_tbl) %>%
  select(where(is.numeric)) %>% 
  plot_hist_facet()

# 4. Dummy variables ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal()) %>% 
  
  # prepare the final recipe
  prep()
train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)

train_tbl %>% glimpse()

test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)

train_tbl %>%
  
  # Convert characters & factors to numeric
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  
  # Correlation
  cor(use = "pairwise.complete.obs") %>% 
  as_tibble() %>%
  mutate(feature = names(.)) %>% 
  select(feature, Attrition_Yes) %>% 
  
  # Filter the target, because we now the correlation is 100%
  filter(!(feature == "Attrition_Yes")) %>% 
  
  # Convert character back to factors
  mutate(across(where(is.character), as_factor))

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_expr) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)
  
  if (fct_reorder) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }
  
  if (fct_rev) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }
  
  return(data_cor)
  
}

data_cor <- train_tbl %>%
  
  # Correlation
  get_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T) %>%
  
  # Create label text
  mutate(feature_name_text = round(Attrition_Yes, digits = 2)) %>%
  
  # Create flags so that we can change the color for poitive and negative
  mutate(Correlation = case_when(
    (Attrition_Yes) >= 0 ~ "Positive",
    TRUE                   ~ "Negative") %>% as.factor())

data_cor %>%
  ggplot(aes(x = Attrition_Yes, y = feature, group = feature)) +
  geom_point(aes(color = Correlation), size = 2) +
  geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = 1) +
  geom_vline(xintercept = 0, color = "black", size = 0.5) +
  expand_limits(x = c(-1, 1)) +
  scale_color_manual(values = c("red", "#2dc6d6")) +
  geom_label(aes(label = feature_name_text), hjust = "outward")

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, 
                     lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 0.5, 
                     color_pos = "#2dc6d6", color_neg = "red") {
  
  feature_expr <- enquo(target)
  
  # Perform correlation analysis
  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())
  
  # Plot analysis
  g <- data_cor %>%
    ggplot(aes(x = !! feature_expr, y = feature, group = feature)) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = "black", size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    scale_color_manual(values = c(color_neg, color_pos)) +
    theme(legend.position = "bottom")
  
  if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
  
  return(g)
  
}
plot_cor(data = train_tbl, target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>% 
  select(Attrition_Yes, contains("JobRole")) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)

# Correlation Evaluation ----

#   1. Descriptive features: age, gender, marital status 
train_tbl %>%
  select(Attrition_Yes, Age, contains("Gender"), 
         contains("MaritalStatus"), NumCompaniesWorked, 
         contains("Over18"), DistanceFromHome) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)


#   2. Employment features: department, job role, job level
train_tbl %>%
  select(Attrition_Yes, contains("employee"), contains("department"), contains("job")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F) 

#   3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel 
train_tbl %>%
  select(Attrition_Yes, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   4. Survey Results: Satisfaction level, WorkLifeBalance 
train_tbl %>%
  select(Attrition_Yes, contains("satisfaction"), contains("life")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   5. Performance Data: Job Involvment, Performance Rating
train_tbl %>%
  select(Attrition_Yes, contains("performance"), contains("involvement")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   6. Work-Life Features 
train_tbl %>%
  select(Attrition_Yes, contains("overtime"), contains("travel")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   7. Training and Education 
train_tbl %>%
  select(Attrition_Yes, contains("training"), contains("education")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   8. Time-Based Features: Years at company, years in current role
train_tbl %>%
  select(Attrition_Yes, contains("years")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)


# H2O modeling
library(h2o)

employee_attrition_tbl          <- read_csv("datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl             <- read_excel("data_definitions.xlsx", sheet = 1, col_names = FALSE)
employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)
set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# Modeling
h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "Attrition"
x <- setdiff(names(train_h2o), y)


automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)

typeof(automl_models_h2o)
## "S4"

slotNames(automl_models_h2o)
## [1] "project_name"   "leader"         "leaderboard"    "event_log"      "modeling_steps" "training_info" 

automl_models_h2o@leaderboard
automl_models_h2o@leader

# Depending on the algorithm, the output will be different
h2o.getModel("DeepLearning_grid__1_AutoML_20200820_190823_model_1")

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(6) %>% 
  h2o.getModel()

h2o.getModel("DeepLearning_grid__1_AutoML_20200820_190823_model_1") %>% 
  h2o.saveModel(path = "04_Modeling/h20_models/")

h2o.loadModel("04_Modeling/h20_models/DeepLearning_grid__1_AutoML_20200820_190823_model_1")

# Choose whatever model you want
stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_AutoML_20200820_190823")
stacked_ensemble_h2o

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions)
## [1] "environment"

predictions_tbl <- predictions %>% as_tibble()
deep_learning_h2o <- h2o.loadModel("04_Modeling/h20_models/DeepLearning_grid__1_AutoML_20200820_190823_model_1")

# To see all possible parameters
?h2o.deeplearning

# to get all paramteres
deep_learning_h2o@allparameters






# Challenge
# Load necessary libraries
library(tidyverse)
library(readr)
library(rsample)
library(h2o)

# Load the data
product_backorders_tbl <- read_csv("scripts/data/product_backorders.csv")

# Check for missing values and handle them
product_backorders_tbl <- product_backorders_tbl %>%
  mutate(across(everything(), ~ ifelse(is.na(.), -1, .)))

# Check and adjust data types
str(product_backorders_tbl)

# Convert categorical variables to factors
product_backorders_tbl <- product_backorders_tbl %>%
  mutate(across(where(is.character), as.factor))

# Split the data into training, validation, and test sets
set.seed(1113)  # Setting seed for reproducibility
split_obj <- rsample::initial_split(product_backorders_tbl, prop = c(0.7))

train_tbl <- training(split_obj)
#valid_tbl <- validation(split_obj)
test_tbl <- testing(split_obj)

# Initialize H2O
h2o.init(max_mem_size = "4G")

# Convert data frames to H2O objects
train_h2o <- as.h2o(train_tbl)
#valid_h2o <- as.h2o(valid_tbl)
test_h2o <- as.h2o(test_tbl)

# Specify the response and predictor variables
y <- "went_on_backorder"
x <- setdiff(names(train_tbl), y)

# Run AutoML
automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  #validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 300,  # 5 minutes
  nfolds            = 5
)

# View the leaderboard
leaderboard <- automl_models_h2o@leaderboard
print(leaderboard)

# Predict using the leader model
leader_model <- automl_models_h2o@leader
predictions <- h2o.predict(leader_model, newdata = test_h2o)

# Save the leader model
model_path <- h2o.saveModel(object = leader_model, path = "scripts/data/h2o_models", force = TRUE)
print(model_path)

# Shutdown H2O
h2o.shutdown(prompt = FALSE)

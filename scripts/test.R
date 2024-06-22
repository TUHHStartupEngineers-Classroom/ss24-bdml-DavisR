# Load Libraries 
library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(lime)
library(rsample)

# Load Data
employee_attrition_tbl <- read_csv("scripts/data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl <- read_excel("scripts/data/data_definitions.xlsx", sheet = 1, col_names = FALSE)

# Processing Pipeline (assuming this file includes your process_hr_data_readable function)
source("scripts/data/data_processing_pipeline.R")

# Process data and split into test and train
set.seed(seed = 1113)
employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.7)
train_readable_tbl <- training(split_obj)
test_readable_tbl <- testing(split_obj)

# Align levels of factor columns in test set with the training set
align_levels <- function(train, test) {
  for (col in colnames(train)) {
    if (is.factor(train[[col]]) && is.factor(test[[col]])) {
      levels_to_use <- levels(train[[col]])
      test[[col]] <- factor(test[[col]], levels = levels_to_use)
    }
  }
  return(test)
}

test_readable_tbl <- align_levels(train_readable_tbl, test_readable_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()

# Apply recipe to train and test sets
train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)

# Initialize H2O
h2o.init()

# Load pre-trained model
automl_leader <- h2o.loadModel("scripts/data/h2o_models/StackedEnsemble_BestOfFamily_4_AutoML_4_20240617_131246")

# Convert data frames to H2O frames
train_h2o <- as.h2o(train_tbl)
test_h2o <- as.h2o(test_tbl)

# 3. LIME ----

# 3.1 Making Predictions ----

# Making Predictions with probabilities
predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = test_h2o) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

predictions_tbl

# 3.2 Single Explanation ----

# Explain prediction for a single observation
explanation <- test_tbl %>%
  slice(1) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = lime::lime(model = automl_leader),
    n_labels = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width = 1
  )

explanation

explanation %>%
  as.tibble() %>%
  select(feature:prediction) 

g <- plot_features(explanation = explanation, ncol = 1)

# 3.3 Multiple Explanations ----

explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer = explainer,
    n_labels   = 1,
    n_features = 8,
    n_permutations = 5000,
    kernel_width   = 0.5
  )

explanation %>%
  as.tibble()

plot_features(explanation, ncol = 4)

plot_explanations(explanation)


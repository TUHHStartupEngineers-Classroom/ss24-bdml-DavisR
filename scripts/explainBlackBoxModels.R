# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)
library(readr)
library(rsample)


# Load Data
employee_attrition_tbl <- read_csv("scripts/data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("scripts/data/data_definitions.xlsx", sheet = 1, col_names = FALSE)

# Processing Pipeline
source("scripts/data/data_processing_pipeline.R")

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)

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

# Apply level alignment before baking the data
test_readable_tbl <- align_levels(train_readable_tbl, test_readable_tbl)


# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)



# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("scripts/data/h2o_models/StackedEnsemble_BestOfFamily_4_AutoML_4_20240617_131246")
automl_leader

# Convert data frames to H2O frames
train_h2o <- as.h2o(train_tbl)
test_h2o <- as.h2o(test_tbl)

# 3. LIME ----

# 3.1 Making Predictions ----

# Making Predictions
predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

predictions_tbl
## # A tibble: 220 x 5
##    predict    No    Yes Attrition EmployeeNumber
##    <fct>   <dbl>  <dbl> <fct>              <dbl>
##  1 Yes     0.363 0.637  Yes                    1
##  2 No      0.863 0.137  No                    15
##  3 No      0.963 0.0374 No                    20
##  4 No      0.868 0.132  No                    21
##  5 No      0.952 0.0483 No                    38
##  6 No      0.808 0.192  No                    49
##  7 No      0.930 0.0696 No                    54
##  8 Yes     0.559 0.441  No                    61
##  9 Yes     0.412 0.588  No                    62
## 10 No      0.936 0.0640 No                    70
## # … with 210 more rows


test_tbl %>%
  slice(1) %>%
  glimpse()

# 3.2 Single Explanation ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model           = automl_leader,
    bin_continuous  = TRUE,
    n_bins          = 4,
    quantile_bins   = TRUE
  )

explainer



explanation <- test_tbl %>%
  slice(1) %>%
  select(-Attrition) %>%
  lime::explain(
    
    # Pass our explainer object
    explainer = explainer,
    # Because it is a binary classification model: 1
    n_labels   = 1,
    # number of features to be returned
    n_features = 8,
    # number of localized linear models
    n_permutations = 5000,
    # Let's start with 1
    kernel_width   = 1
  )

explanation

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

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>%
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)




# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("scripts/data/h2o_models/DeepLearning_grid_1_AutoML_1_20240622_153440_model_1")
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

explanation %>%
  as.tibble() %>%
  select(feature:prediction) 

g <- plot_features(explanation = explanation, ncol = 1)
g
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


# Challenge
# Custom plot function for a single case
plot_features_custom <- function(explanation_data, case_id = 1) {
  case_data <- explanation_data %>% filter(case == case_id)
  
  case_data %>%
    mutate(feature_desc = factor(feature_desc, levels = rev(feature_desc)),  # Ensure correct ordering
           label = ifelse(feature_weight > 0, "Supports", "Contradicts"),
           color = ifelse(label == "Supports", "#4682b4", "#b22222")) %>%
    ggplot(aes(x = feature_desc, y = feature_weight, fill = label)) +
    geom_col(color = "black") +
    coord_flip() +
    labs(x = "Feature", y = "Weight") +
    scale_fill_manual(values = c("Supports" = "#4682b4", "Contradicts" = "#b22222")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
    #geom_text(aes(label = feature_weight), position = position_stack(vjust = 0.5), color = "white") +
    ggtitle(paste0("Case: ", case_id, "\n",
                   "Label: ", unique(case_data$label), "\n",
                   "Probability: ", round(unique(case_data$label_prob), 2), "\n",
                   "Explanation Fit: ", round(unique(case_data$model_r2), 2))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
}

# Example usage with your explanation data
plot_features_custom(explanation, case_id = 1)


# Define a custom plot function for plot_explanations()
plot_explanations_custom <- function(explanation, ...) {
  explanation$feature_desc <- factor(
    explanation$feature_desc,
    levels = rev(unique(
      explanation$feature_desc[
        order(explanation$feature, explanation$feature_value)
        ]))
  )

  num_cases <- unique(suppressWarnings(as.numeric(explanation$case)))
  if (!anyNA(num_cases)) {
    explanation$case <- factor(explanation$case, levels = as.character(sort(num_cases)))
  }
  p <- ggplot(explanation, aes_(~case, ~feature_desc)) +
    geom_tile(aes_(fill = ~feature_weight)) +
    scale_fill_gradient2('Feature\nweight', low = '#b22222', mid = 'white', high = '#4682b4') +
    scale_y_discrete('Feature', expand = c(0, 0)) +
    scale_x_discrete('Case', expand = c(0, 0)) +
    theme_light() +
    theme(panel.border = element_rect(fill = NA, colour = 'grey', size = 1),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  p + facet_wrap(~label)
}

# Use this function with your explanation data
plot_explanations_custom(explanation)

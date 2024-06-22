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

# Visualize the H2O leaderboard to help with model selection
data_transformed_tbl <- automl_models_h2o@leaderboard %>%
  as_tibble() %>%
  select(-c(aucpr, mean_per_class_error, rmse, mse)) %>% 
  mutate(model_type = str_extract(model_id, "[^_]+")) %>%
  slice(1:15) %>% 
  rownames_to_column(var = "rowname") %>%
  # Visually this step will not change anything
  # It reorders the factors under the hood
  mutate(
    model_id   = as_factor(model_id) %>% reorder(auc),
    model_type = as.factor(model_type)
  ) %>% 
  pivot_longer(cols = -c(model_id, model_type, rowname), 
               names_to = "key", 
               values_to = "value", 
               names_transform = list(key = forcats::fct_inorder)
  ) %>% 
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev())

# Predict using the leader model
leader_model <- automl_models_h2o@leader
predictions <- h2o.predict(leader_model, newdata = test_h2o)

# Convert predictions to a data frame
predictions_tbl <- as_tibble(predictions)
print(predictions_tbl)
data_transformed_tbl %>%
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  
  # Facet to break out logloss and auc
  facet_wrap(~ key, scales = "free_x") +
  labs(title = "Leaderboard Metrics",
       subtitle = paste0("Ordered by: ", "auc"),
       y = "Model Postion, Model ID", x = "") + 
  theme(legend.position = "bottom")

plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) {
  
  # Setup inputs
  # adjust input so that all formats are working
  order_by <- tolower(order_by[[1]])
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    select(-c(aucpr, mean_per_class_error, rmse, mse)) %>% 
    mutate(model_type = str_extract(model_id, "[^_]+")) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(model_id = paste0(rowname, ". ", model_id) %>% as.factor())
  
  # Transformation
  if (order_by == "auc") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(auc),
        model_type = as.factor(model_type)
      ) %>%
      pivot_longer(cols = -c(model_id, model_type, rowname), 
                   names_to = "key", 
                   values_to = "value", 
                   names_transform = list(key = forcats::fct_inorder)
      )
    
  } else if (order_by == "logloss") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
        model_type = as.factor(model_type)
      ) %>%
      pivot_longer(cols = -c(model_id, model_type, rowname), 
                   names_to = "key", 
                   values_to = "value", 
                   names_transform = list(key = forcats::fct_inorder)
      )
    
  } else {
    # If nothing is supplied
    stop(paste0("order_by = '", order_by, "' is not a permitted option."))
  }
  
  # Visualization
  g <- data_transformed_tbl %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~ key, scales = "free_x") +
    labs(title = "Leaderboard Metrics",
         subtitle = paste0("Ordered by: ", toupper(order_by)),
         y = "Model Postion, Model ID", x = "")
  
  if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), 
                                           hjust = "inward"))
  
  return(g)
  
}

deeplearning_h2o <- h2o.loadModel("scripts/data/h2o_models/StackedEnsemble_BestOfFamily_4_AutoML_4_20240617_131246")

# Take a look for the metrics on the training data set
# For my model the total error in the confusion matrix is ~15 %
deeplearning_h2o

# We want to see how it performs for the testing data frame
test_tbl

# Make sure to convert it to an h20 object
# Accuracy of the confusion matrix shows ~85 % accuracy
h2o.performance(deeplearning_h2o, newdata = as.h2o(test_tbl))

deeplearning_grid_01 <- h2o.grid(
  
  # See help page for available algos
  algorithm = "deeplearning",
  
  # I just use the same as the object
  grid_id = "deeplearning_grid_01",
  
  # The following is for ?h2o.deeplearning()
  # predictor and response variables
  x = x,
  y = y,
  
  # training and validation frame and crossfold validation
  training_frame   = train_h2o,
  #validation_frame = valid_h2o,
  nfolds = 5,
  
  # Hyperparamters: Use deeplearning_h2o@allparameters to see all
  hyper_params = list(
    # Use some combinations (the first one was the original)
    hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
    epochs = c(10, 50, 100)
  )
)

deeplearning_grid_01

h2o.getGrid(grid_id = "deeplearning_grid_01", sort_by = "auc", decreasing = TRUE)


deeplearning_grid_01_model_1 <- h2o.getModel("deeplearning_grid_01_model_1")

deeplearning_grid_01_model_1 %>% h2o.auc(train = T, valid = T, xval = T)


# We can tell the model is overfitting because of the huge difference between training AUC and the validation / cross validation AUC

# Run it on the test data
deeplearning_grid_01_model_1 %>%
  h2o.performance(newdata = as.h2o(test_tbl))

# 4. Assessing Performance ----
stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_AllModels_AutoML_20200826_112031")
deeplearning_h2o     <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_grid__1_AutoML_20200826_112031_model_1")
glm_h2o              <- h2o.loadModel("04_Modeling/h2o_models/GLM_1_AutoML_20200826_112031")

performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(performance_h2o)
performance_h2o %>% slotNames()

# We are focusing on the slot metrics. This slot contains all possible metrics
performance_h2o@metrics

# Classifier Summary Metrics

h2o.auc(performance_h2o, train = T, valid = T, xval = T)
## [1] 0.8588763

# Caution: "train, "val", and "xval" arugments only work for models (not performance objects)
h2o.auc(stacked_ensemble_h2o, train = T, valid = T, xval = T)
##     train     valid      xval 
## 0.9892475 0.8219522 0.8383290 

h2o.giniCoef(performance_h2o)
## [1] 0.7177527
h2o.logloss(performance_h2o)
## [1] 0.2941769

# result for the training data
h2o.confusionMatrix(stacked_ensemble_h2o)
## Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.358554294328892:
##         No Yes    Error      Rate
## No     871  20 0.022447   =20/891
## Yes     23 151 0.132184   =23/174
## Totals 894 171 0.040376  =43/1065

# result for the hold out set
h2o.confusionMatrix(performance_h2o)

# Precision vs Recall Plot

# This is on the test set
performance_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as.tibble() 

theme_new <- theme(
  legend.position  = "bottom",
  legend.key       = element_blank(),,
  panel.background = element_rect(fill   = "transparent"),
  panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
  panel.grid.major = element_line(color = "grey", size = 0.333)
) 

performance_tbl %>%
  filter(f1 == max(f1))

performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  
  # Insert line where precision and recall are harmonically optimized
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")) +
  labs(title = "Precision vs Recall", y = "value") +
  theme_new

# ROC Plot

path <- "04_Modeling/h2o_models/StackedEnsemble_AllModels_AutoML_20200826_112031"

load_model_performance_metrics <- function(path, test_tbl) {
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
  
  perf_h2o %>%
    h2o.metric() %>%
    as_tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc)
  
}

model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest(cols = metrics)

model_metrics_tbl %>%
  mutate(
    # Extract the model names
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(fpr, tpr, color = path, linetype = auc)) +
  geom_line(size = 1) +
  
  # just for demonstration purposes
  geom_abline(color = "red", linetype = "dotted") +
  
  theme_new +
  theme(
    legend.direction = "vertical",
  ) +
  labs(
    title = "ROC Plot",
    subtitle = "Performance of 3 Top Performing Models"
  )

# Precision vs Recall

load_model_performance_metrics <- function(path, test_tbl) {
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
  
  perf_h2o %>%
    h2o.metric() %>%
    as_tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc, precision, recall)
  
}

model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest(cols = metrics)

model_metrics_tbl %>%
  mutate(
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(recall, precision, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_new + 
  theme(
    legend.direction = "vertical",
  ) +
  labs(
    title = "Precision vs Recall Plot",
    subtitle = "Performance of 3 Top Performing Models"
  )

# Gain & Lift

ranked_predictions_tbl <- predictions_tbl %>%
  bind_cols(test_tbl) %>%
  select(predict:Yes, Attrition) %>%
  # Sorting from highest to lowest class probability
  arrange(desc(Yes))
## # A tibble: 220 x 4
##    predict     No   Yes Attrition
##    <fct>    <dbl> <dbl> <fct>    
##  1 Yes     0.0113 0.989 Yes      
##  2 Yes     0.0342 0.966 Yes      
##  3 Yes     0.0431 0.957 Yes      
##  4 Yes     0.0777 0.922 Yes      
##  5 Yes     0.143  0.857 No       
##  6 Yes     0.160  0.840 Yes      
##  7 Yes     0.167  0.833 Yes      
##  8 Yes     0.191  0.809 Yes      
##  9 Yes     0.235  0.765 No       
## 10 Yes     0.257  0.743 Yes      
## # … with 210 more rows

ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarise(
    cases = n(),
    responses = sum(Attrition == "Yes")
  ) %>%
  arrange(desc(ntile))
## `summarise()` ungrouping output (override with `.groups` argument)
## # A tibble: 10 x 3
##    ntile cases responses
##    <int> <int>     <int>
##  1    10    22        15
##  2     9    22         6
##  3     8    22         2
##  4     7    22         3
##  5     6    22         3
##  6     5    22         2
##  7     4    22         0
##  8     3    22         1
##  9     2    22         0
## 10     1    22         0


calculated_gain_lift_tbl <- ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarise(
    cases = n(),
    responses = sum(Attrition == "Yes")
  ) %>%
  arrange(desc(ntile)) %>%
  
  # Add group numbers (opposite of ntile)
  mutate(group = row_number()) %>%
  select(group, cases, responses) %>%
  
  # Calculations
  mutate(
    cumulative_responses = cumsum(responses),
    pct_responses        = responses / sum(responses),
    gain                 = cumsum(pct_responses),
    cumulative_pct_cases = cumsum(cases) / sum(cases),
    lift                 = gain / cumulative_pct_cases,
    gain_baseline        = cumulative_pct_cases,
    lift_baseline        = gain_baseline / cumulative_pct_cases
  )

calculated_gain_lift_tbl 
## # A tibble: 10 x 10
##    group cases responses cumulative_responses pct_responses  gain cumulative_pct_cases  lift gain_baseline lift_baseline
##    <int> <int>     <int>                <int>         <dbl> <dbl>                <dbl> <dbl>         <dbl>         <dbl>
##  1     1    22        15                   15        0.469  0.469                  0.1  4.69           0.1             1
##  2     2    22         6                   21        0.188  0.656                  0.2  3.28           0.2             1
##  3     3    22         2                   23        0.0625 0.719                  0.3  2.40           0.3             1
##  4     4    22         3                   26        0.0938 0.812                  0.4  2.03           0.4             1
##  5     5    22         3                   29        0.0938 0.906                  0.5  1.81           0.5             1
##  6     6    22         2                   31        0.0625 0.969                  0.6  1.61           0.6             1
##  7     7    22         0                   31        0      0.969                  0.7  1.38           0.7             1
##  8     8    22         1                   32        0.0312 1                      0.8  1.25           0.8             1
##  9     9    22         0                   32        0      1                      0.9  1.11           0.9             1
## 10    10    22         0                   32        0      1                      1    1              1               1


gain_lift_tbl <- performance_h2o %>%
  h2o.gainsLift() %>%
  as.tibble()

## Gain Chart

gain_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain     = cumulative_capture_rate) %>%
  # prepare the data for the plotting (for the color and group aesthetics)
  pivot_longer(cols = c(gain, baseline), values_to = "value", names_to = "key")

gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  labs(
    title = "Gain Chart",
    x = "Cumulative Data Fraction",
    y = "Gain"
  ) +
  theme_new

## Lift Plot

lift_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("capture")) %>%
  mutate(baseline = 1) %>%
  rename(lift = cumulative_lift) %>%
  pivot_longer(cols = c(lift, baseline), values_to = "value", names_to = "key")

lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  labs(
    title = "Lift Chart",
    x = "Cumulative Data Fraction",
    y = "Lift"
  ) +
  theme_new

# 5. Performance Visualization ----  
library(cowplot)
library(glue)


# set values to test the function while building it
h2o_leaderboard <- automl_models_h2o@leaderboard
newdata <- test_tbl
order_by <- "auc"
max_models <- 4
size <- 1

plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
  
  # Inputs
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as_tibble()
  
  # Selecting the first, if nothing is provided
  order_by      <- tolower(order_by[[1]]) 
  
  # Convert string stored in a variable to column name (symbol)
  order_by_expr <- rlang::sym(order_by)
  
  # Turn of the progress bars ( opposite h2o.show_progress())
  h2o.no_progress()
  
  # 1. Model metrics
  
  get_model_performance_metrics <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as.tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
    
  }
  
  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
    unnest(cols = metrics) %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        # programmatically reorder factors depending on order_by
        fct_reorder(!! order_by_expr, 
                    .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc      = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss  = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    )
  
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes(fpr, tpr, color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    theme_new +
    labs(title = "ROC", x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical") 
  
  
  # 1B. Precision vs Recall
  
  p2 <- model_metrics_tbl %>%
    ggplot(aes(recall, precision, color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    theme_new +
    labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
    theme(legend.position = "none") 
  
  
  # 2. Gain / Lift
  
  get_gain_lift <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
    
    perf_h2o %>%
      h2o.gainsLift() %>%
      as.tibble() %>%
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
    
  }
  
  gain_lift_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
    unnest(cols = metrics) %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, 
                    .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    ) %>%
    rename(
      gain = cumulative_capture_rate,
      lift = cumulative_lift
    ) 
  
  # 2A. Gain Plot
  
  p3 <- gain_lift_tbl %>%
    ggplot(aes(cumulative_data_fraction, gain, 
               color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size,) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 color = "red", size = size, linetype = "dotted") +
    theme_new +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Gain",
         x = "Cumulative Data Fraction", y = "Gain") +
    theme(legend.position = "none")
  
  # 2B. Lift Plot
  
  p4 <- gain_lift_tbl %>%
    ggplot(aes(cumulative_data_fraction, lift, 
               color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                 color = "red", size = size, linetype = "dotted") +
    theme_new +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Lift",
         x = "Cumulative Data Fraction", y = "Lift") +
    theme(legend.position = "none") 
  
  
  # Combine using cowplot
  
  # cowplot::get_legend extracts a legend from a ggplot object
  p_legend <- get_legend(p1)
  # Remove legend from p1
  p1 <- p1 + theme(legend.position = "none")
  
  # cowplot::plt_grid() combines multiple ggplots into a single cowplot object
  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
  
  # cowplot::ggdraw() sets up a drawing layer
  p_title <- ggdraw() + 
    
    # cowplot::draw_label() draws text on a ggdraw layer / ggplot object
    draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
               color = "#2C3E50")
  
  p_subtitle <- ggdraw() + 
    draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,  
               color = "#2C3E50")
  
  # Combine everything
  ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                   
                   # Adjust the relative spacing, so that the legends always fits
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
  
  h2o.show_progress()
  
  return(ret)
  
}

automl_models_h2o@leaderboard %>%
  plot_h2o_performance(newdata = test_tbl, order_by = "logloss", 
                       size = 0.5, max_models = 4)







# Challenge
# Load necessary libraries
library(tidyverse)
library(readr)
library(rsample)
library(h2o)
library(cowplot)
library(glue)

# Functions
theme_new <- theme(
  legend.position  = "bottom",
  legend.key       = element_blank(),,
  panel.background = element_rect(fill   = "transparent"),
  panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
  panel.grid.major = element_line(color = "grey", size = 0.333)
) 

plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
  
  # Inputs
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as_tibble()
  
  # Selecting the first, if nothing is provided
  order_by      <- tolower(order_by[[1]]) 
  
  # Convert string stored in a variable to column name (symbol)
  order_by_expr <- rlang::sym(order_by)
  
  # Turn of the progress bars ( opposite h2o.show_progress())
  h2o.no_progress()
  
  # 1. Model metrics
  
  get_model_performance_metrics <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as.tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
    
  }
  
  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
    unnest(cols = metrics) %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        # programmatically reorder factors depending on order_by
        fct_reorder(!! order_by_expr, 
                    .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc      = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss  = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    )
  
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes(fpr, tpr, color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    theme_new +
    labs(title = "ROC", x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical") 
  
  
  # 1B. Precision vs Recall
  
  p2 <- model_metrics_tbl %>%
    ggplot(aes(recall, precision, color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    theme_new +
    labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
    theme(legend.position = "none") 
  
  
  # 2. Gain / Lift
  
  get_gain_lift <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
    
    perf_h2o %>%
      h2o.gainsLift() %>%
      as.tibble() %>%
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
    
  }
  
  gain_lift_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
    unnest(cols = metrics) %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, 
                    .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    ) %>%
    rename(
      gain = cumulative_capture_rate,
      lift = cumulative_lift
    ) 
  
  # 2A. Gain Plot
  
  p3 <- gain_lift_tbl %>%
    ggplot(aes(cumulative_data_fraction, gain, 
               color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size,) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 color = "red", size = size, linetype = "dotted") +
    theme_new +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Gain",
         x = "Cumulative Data Fraction", y = "Gain") +
    theme(legend.position = "none")
  
  # 2B. Lift Plot
  
  p4 <- gain_lift_tbl %>%
    ggplot(aes(cumulative_data_fraction, lift, 
               color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                 color = "red", size = size, linetype = "dotted") +
    theme_new +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Lift",
         x = "Cumulative Data Fraction", y = "Lift") +
    theme(legend.position = "none") 
  
  
  # Combine using cowplot
  
  # cowplot::get_legend extracts a legend from a ggplot object
  p_legend <- get_legend(p1)
  # Remove legend from p1
  p1 <- p1 + theme(legend.position = "none")
  
  # cowplot::plt_grid() combines multiple ggplots into a single cowplot object
  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
  
  # cowplot::ggdraw() sets up a drawing layer
  p_title <- ggdraw() + 
    
    # cowplot::draw_label() draws text on a ggdraw layer / ggplot object
    draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
               color = "#2C3E50")
  
  p_subtitle <- ggdraw() + 
    draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,  
               color = "#2C3E50")
  
  # Combine everything
  ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                   
                   # Adjust the relative spacing, so that the legends always fits
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
  
  h2o.show_progress()
  
  return(ret)
}
  

# Load the data
product_backorders_tbl <- read_csv("scripts/data/product_backorders.csv")

# Check for missing values and handle them
product_backorders_tbl <- product_backorders_tbl %>%
  mutate(across(everything(), ~ ifelse(is.na(.), -1, .)))

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


# Run AutoML
automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  leaderboard_frame = test_h2o,  # Using test data for leaderboard
  max_runtime_secs = 300,  # 5 minutes
  nfolds = 5
)

# View the leaderboard
leaderboard <- automl_models_h2o@leaderboard
print(leaderboard)



# Visualize the leaderboard
plot_h2o_performance(automl_models_h2o@leaderboard, newdata = test_tbl, order_by = "logloss", max_models = 5)


# Assuming you want to tune a deep learning model with grid search
# Define parameters for grid search
hyper_params <- list(
  hidden = list(c(10, 10), c(20, 20), c(30, 30)),
  epochs = c(10, 20, 30)
)


# Perform grid search
deeplearning_grid <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "deeplearning_grid_2",
  x = x,
  y = y,
  training_frame = train_h2o,
  #validation_frame = valid_h2o,
  nfolds = 5,
  hyper_params = list(
    # Use some combinations (the first one was the original)
    hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
    epochs = c(10, 50, 100)
  )
)

# Get the best model from grid search
best_model <- h2o.getModel(deeplearning_grid@model_ids[[1]]) %>%
  h2o.saveModel(path = "scripts/data/h2o_models/")


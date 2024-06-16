library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(broom.mixed) # for converting bayesian models to tidy tibbles

# Data set
bike_data_tbl <- readRDS("scripts/data/bike_orderlines.rds")

ggplot(bike_data_tbl,
       aes(x = price, 
           y = weight, 
           group = category_1, 
           col = category_1)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_manual(values=c("#2dc6d6", "#d65a2d", "#d6af2d", "#8a2dd6", "#1a2db6"))

lm_mod <- linear_reg() %>% 
  set_engine("lm")

lm_fit <- lm_mod %>% 
  fit(weight ~ price * category_1, 
      data = bike_data_tbl)

new_points <- expand.grid(price = 20000, 
                          category_1 = c("E-Bikes", "Hybrid / City", "Mountain", "Road"))
mean_pred <- predict(lm_fit, new_data = new_points)

conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")

# Now combine: 
plot_data <- new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = category_1)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "Bike weight", x = "Category") 

# set the prior distribution
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# make the parsnip model
bayes_mod <- linear_reg() %>% 
  set_engine("stan",
             prior_intercept = prior_dist, 
             prior = prior_dist) 

# train the model
bayes_fit <-  bayes_mod %>% 
  fit(weight ~ price * category_1, 
      data = bike_data_tbl)

print(bayes_fit, digits = 5)


bayes_plot_data <- 
  new_points %>%
  bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(bayes_plot_data, aes(x = category_1)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
  labs(y = "Bike weight") + 
  ggtitle("Bayesian model with t(1) prior distribution")


set.seed(123)

flight_data <- 
  flights %>% 
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = as.Date(time_hour)
  ) %>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)


flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))



# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(555)
# Put 3/4 of the data into the training set 
data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)


flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) 

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID")


flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_rm(date)

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>% 
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_rm(date) %>% 
  step_dummy(all_nominal(), -all_outcomes())







# Supervised ML

# Business case
# Standard
library(tidyverse)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

# Modeling ----------------------------------------------------------------
bike_orderlines_tbl <- readRDS("scripts/data/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

model_sales_tbl <- bike_orderlines_tbl %>%
  select(total_price, model, category_2, frame_material) %>%
  
  group_by(model, category_2, frame_material) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  arrange(desc(total_sales))

model_sales_tbl %>%
  mutate(category_2 = as_factor(category_2) %>% 
           fct_reorder(total_sales, .fun = max) %>% 
           fct_rev()) %>%
  
  ggplot(aes(frame_material, total_sales)) +
  geom_violin() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
  #coord_flip() +
  facet_wrap(~ category_2) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
  tidyquant::theme_tq() +
  labs(
    title = "Total Sales for Each Model",
    x = "Frame Material", y = "Revenue"
  )

bike_features_tbl <- readRDS("scripts/data/bike_features_tbl.rds")
glimpse(bike_features_tbl)

bike_features_tbl <- bike_features_tbl %>% 
  select(model:url, `Rear Derailleur`, `Shift Lever`) %>% 
  mutate(
    `shimano dura-ace`        = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano dura-ace ") %>% as.numeric(),
    `shimano ultegra`         = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano ultegra ") %>% as.numeric(),
    `shimano 105`             = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano 105 ") %>% as.numeric(),
    `shimano tiagra`          = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano tiagra ") %>% as.numeric(),
    `Shimano sora`            = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano sora") %>% as.numeric(),
    `shimano deore`           = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano deore(?! xt)") %>% as.numeric(),
    `shimano slx`             = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano slx") %>% as.numeric(),
    `shimano grx`             = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano grx") %>% as.numeric(),
    `Shimano xt`              = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano deore xt |shimano xt ") %>% as.numeric(),
    `Shimano xtr`             = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano xtr") %>% as.numeric(),
    `Shimano saint`           = `Rear Derailleur` %>% str_to_lower() %>% str_detect("shimano saint") %>% as.numeric(),
    `SRAM red`                = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram red") %>% as.numeric(),
    `SRAM force`              = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram force") %>% as.numeric(),
    `SRAM rival`              = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram rival") %>% as.numeric(),
    `SRAM apex`               = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram apex") %>% as.numeric(),
    `SRAM xx1`                = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram xx1") %>% as.numeric(),
    `SRAM x01`                = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram x01|sram xo1") %>% as.numeric(),
    `SRAM gx`                 = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram gx") %>% as.numeric(),
    `SRAM nx`                 = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram nx") %>% as.numeric(),
    `SRAM sx`                 = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram sx") %>% as.numeric(),
    `SRAM sx`                 = `Rear Derailleur` %>% str_to_lower() %>% str_detect("sram sx") %>% as.numeric(),
    `Campagnolo potenza`      = `Rear Derailleur` %>% str_to_lower() %>% str_detect("campagnolo potenza") %>% as.numeric(),
    `Campagnolo super record` = `Rear Derailleur` %>% str_to_lower() %>% str_detect("campagnolo super record") %>% as.numeric(),
    `shimano nexus`           = `Shift Lever`     %>% str_to_lower() %>% str_detect("shimano nexus") %>% as.numeric(),
    `shimano alfine`          = `Shift Lever`     %>% str_to_lower() %>% str_detect("shimano alfine") %>% as.numeric()
  ) %>% 
  # Remove original columns  
  select(-c(`Rear Derailleur`, `Shift Lever`)) %>% 
  # Set all NAs to 0
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# 2.0 TRAINING & TEST SETS ----
bike_features_tbl <- bike_features_tbl %>% 
  
  mutate(id = row_number()) %>% 
  
  select(id, everything(), -url)


bike_features_tbl %>% distinct(category_2)

# run both following commands at the same time
set.seed(seed = 1113)
split_obj <- rsample::initial_split(bike_features_tbl, prop   = 0.80, 
                                    strata = "category_2")

# Check if testing contains all category_2 values
split_obj %>% training() %>% distinct(category_2)
split_obj %>% testing() %>% distinct(category_2)

# Assign training and test data
train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

# We have to remove spaces and dashes from the column names
train_tbl <- train_tbl %>% set_names(str_replace_all(names(train_tbl), " |-", "_"))
test_tbl  <- test_tbl  %>% set_names(str_replace_all(names(test_tbl),  " |-", "_"))

# 3.0 LINEAR METHODS ----
# 3.1 LINEAR REGRESSION - NO ENGINEERED FEATURES ----

# 3.1.1 Model ----
?lm # from the stats package
?set_engine
?fit # then click Estimate model parameters and then fit at the bottom

model_01_linear_lm_simple <- linear_reg(mode = "regression") %>%
  set_engine("lm") %>%
  fit(price ~ category_2 + frame_material, data = train_tbl)


model_01_linear_lm_simple %>%
  predict(new_data = test_tbl)

model_01_linear_lm_simple %>%
  predict(new_data = test_tbl) %>%
  
  bind_cols(test_tbl %>% select(price)) %>%
  
  # Manual approach
  # mutate(residuals = price - .pred) %>% 
  # 
  # summarize(
  #   mae  = abs(residuals) %>% mean(),
  #   rmse = mean(residuals^2)^0.5
  # )
  
  yardstick::metrics(truth = price, estimate = .pred)


# 3.1.2 Feature Importance ----
View(model_01_linear_lm_simple) # You will see the coefficients in the element "fit"

# tidy() function is applicable for objects with class "lm"
model_01_linear_lm_simple$fit %>% class()

model_01_linear_lm_simple$fit %>%
  broom::tidy() %>%
  arrange(p.value) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>%
  
  ggplot(aes(x = estimate, y = term)) +
  geom_point(color = "#2dc6d6", size = 3) +
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1, suffix = " €", prefix = "")),
                            size = 4, fill = "#272A36", color = "white") +
  scale_x_continuous(labels = scales::dollar_format(suffix = " €", prefix = "")) +
  labs(title = "Linear Regression: Feature Importance",
       subtitle = "Model 01: Simple lm Model") 


# 3.1.3 Function to Calculate Metrics ----

# Code we used earlier
model_01_linear_lm_simple %>%
  predict(new_data = test_tbl) %>%
  
  bind_cols(test_tbl %>% select(price)) %>%
  yardstick::metrics(truth = price, estimate = .pred)

# Generalized into a function
calc_metrics <- function(model, new_data = test_tbl) {
  
  model %>%
    predict(new_data = new_data) %>%
    
    bind_cols(new_data %>% select(price)) %>%
    yardstick::metrics(truth = price, estimate = .pred)
  
}

model_01_linear_lm_simple %>% calc_metrics(test_tbl)

# 3.2 LINEAR REGRESSION - WITH ENGINEERED FEATURES ----

# 3.2.1 Model ----
model_02_linear_lm_complex <- linear_reg("regression") %>%
  set_engine("lm") %>%
  
  # This is going to be different. Remove unnecessary columns.
  fit(price ~ ., data = train_tbl %>% select(-c(id:weight), -category_1, -c(category_3:gender)))

model_02_linear_lm_complex %>% calc_metrics(new_data = test_tbl)

# 3.2.2 Feature importance ----
model_02_linear_lm_complex$fit %>%
  broom::tidy() %>%
  arrange(p.value) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>%
  
  ggplot(aes(x = estimate, y = term)) +
  geom_point(color = "#2dc6d6", size = 3) +
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1, suffix = " €", prefix = "")),
                            size = 4, fill = "#272A36", color = "white") +
  scale_x_continuous(labels = scales::dollar_format(suffix = " €", prefix = "")) +
  labs(title = "Linear Regression: Feature Importance",
       subtitle = "Model 02: Complex lm Model")


# 3.3 PENALIZED REGRESSION ----

# 3.3.1 Model ----
?linear_reg
?glmnet::glmnet

model_03_linear_glmnet <- linear_reg(mode    = "regression", 
                                     penalty = 10, 
                                     mixture = 0.1) %>%
  set_engine("glmnet") %>%
  fit(price ~ ., data = train_tbl %>% select(-c(id:weight), -category_1, -c(category_3:gender)))

model_03_linear_glmnet %>% calc_metrics(test_tbl)

# 3.3.2 Feature Importance ----
model_03_linear_glmnet$fit %>%
  broom::tidy() %>%
  filter(lambda >= 10 & lambda < 11) %>%
  
  # No p value here
  arrange(desc(abs(estimate))) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>%
  
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                            size = 3) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Linear Regression: Feature Importance",
       subtitle = "Model 03: GLMNET Model")
# 4.0 TREE-BASED METHODS ----
# 4.1 DECISION TREES ----
# 4.1.1 Model ----

model_04_tree_decision_tree <- decision_tree(mode = "regression",
                                             
                                             # Set the values accordingly to get started
                                             cost_complexity = 0.001,
                                             tree_depth      = 5,
                                             min_n           = 7) %>%
  
  set_engine("rpart") %>%
  fit(price ~ ., data = train_tbl %>% select(-c(id:weight), -category_1, -c(category_3:gender)))

model_04_tree_decision_tree %>% calc_metrics(test_tbl)

model_04_tree_decision_tree$fit %>%
  rpart.plot(roundint = FALSE)

# Optimze plot
model_04_tree_decision_tree$fit %>%
  rpart.plot(
    roundint = FALSE,
    type = 1,
    extra = 101, # see help page
    fallen.leaves = FALSE, # changes the angles from 90 to 45-degree
    cex = 0.8, # font size
    main = "Model 04: Decision Tree", # Adds title
    box.palette = "Blues"
  )

show.prp.palettes()

# 4.2 RANDOM FOREST ----

# 4.2.1 Model: ranger ----
?rand_forest()


library(ranger)

set.seed(1234)
model_05_rand_forest_ranger <- rand_forest(
  mode = "regression", mtry = 8, trees = 5000, min_n = 10
) %>%
  
  # Run ?ranger::ranger to play around with many arguments
  # We need to set importance to impurity to be able to explain the model in the next step
  set_engine("ranger", replace = TRUE, splitrule = "extratrees", importance = "impurity") %>%
  fit(price ~ ., data = train_tbl %>% select(-c(id:weight), -category_1, -c(category_3:gender)))

model_05_rand_forest_ranger %>% calc_metrics(test_tbl)

# 4.2.2 ranger: Feature Importance ----

model_05_rand_forest_ranger$fit %>%
  ranger::importance() %>%
  enframe() %>%
  arrange(desc(value)) %>%
  mutate(name = as_factor(name) %>% fct_rev()) %>%
  
  ggplot(aes(value, name)) +
  geom_point() +
  labs(title = "ranger: Variable Importance",
       subtitle = "Model 05: Ranger Random Forest Model")
# 4.2.3 Model randomForest ----
?rand_forest()
?randomForest::randomForest

set.seed(1234)
model_06_rand_forest_randomForest <- rand_forest("regression") %>%
  set_engine("randomForest") %>%
  
  # All character variables have to be changed to factor variables
  fit(price ~ ., data = train_tbl %>% select(-c(id:weight), -category_1, -c(category_3:gender)) %>% mutate_if(is.character, as_factor))

model_06_rand_forest_randomForest %>% calc_metrics(test_tbl)

# 4.2.4 randomForest: Feature Importance ----

model_06_rand_forest_randomForest$fit %>%
  randomForest::importance() %>%
  # Convert matrix to tibble
  as_tibble(rownames = "name") %>%
  arrange(desc(IncNodePurity)) %>%
  mutate(name = as_factor(name) %>% fct_rev()) %>%
  
  ggplot(aes(IncNodePurity, name)) +
  geom_point() +
  labs(
    title = "randomForest: Variable Importance",
    subtitle = "Model 06: randomForest Model"
  )

# 4.3 XGBOOST ----
# 4.3.1 Model ----

set.seed(1234)
model_07_boost_tree_xgboost <- boost_tree(
  mode = "regression",
  mtry = 30,
  learn_rate = 0.25,
  tree_depth = 7
) %>%
  set_engine("xgboost") %>%
  fit(price ~ ., data = train_tbl %>% select(-c(id:weight), -category_1, -c(category_3:gender)))

model_07_boost_tree_xgboost %>% calc_metrics(test_tbl)

# 4.3.2 Feature Importance ----

model_07_boost_tree_xgboost$fit %>%
  xgboost::xgb.importance(model = .) %>%
  as_tibble() %>%
  arrange(desc(Gain)) %>%
  mutate(Feature = as_factor(Feature) %>% fct_rev()) %>%
  
  ggplot(aes(Gain, Feature)) +
  geom_point() +
  labs(
    title = "XGBoost: Variable Importance",
    subtitle = "Model 07: XGBoost Model"
  )


# 5.0 TESTING THE ALGORITHMS OUT ----
g1 <- bike_features_tbl %>% 
  mutate(category_2 = as.factor(category_2) %>% 
           fct_reorder(price)) %>% 
  
  ggplot(aes(category_2, price)) +
  geom_violin() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#2dc6d6") +
  coord_flip() +
  facet_wrap(~ frame_material) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Unit Price for Each Model",
    y = "", x = "Category 2"
  )


# 5.1 NEW MODEL ----

new_cross_country <- tibble(
  model = "Exceed AL SL new",
  category_2 = "Cross-Country",
  frame_material = "aluminium",
  shimano_dura_ace = 0,
  shimano_ultegra = 0,
  shimano_105 = 0,
  shimano_tiagra = 0,
  Shimano_sora = 0,
  shimano_deore = 0,
  shimano_slx = 0,
  shimano_grx = 0,
  Shimano_xt = 1,
  Shimano_xtr = 0,
  Shimano_saint = 0,
  SRAM_red = 0,
  SRAM_force = 0,
  SRAM_rival = 0,
  SRAM_apex = 0,
  SRAM_xx1 = 0,
  SRAM_x01 = 0,
  SRAM_gx = 0,
  SRAM_nx = 0,
  SRAM_sx = 0,
  Campagnolo_potenza = 0,
  Campagnolo_super_record = 0,
  shimano_nexus = 0,
  shimano_alfine = 0
) 

new_cross_country

# Linear Methods ----
  # Doesn't work right now
  predict(model_01_linear_lm_simple,, new_data = new_cross_country)

# Tree-Based Methods ----

predict(model_07_boost_tree_xgboost, new_data = new_cross_country)

# Iteration
models_tbl <- tibble(
  model_id = str_c("Model 0", 1:7),
  model = list(
    model_01_linear_lm_simple,
    model_02_linear_lm_complex,
    model_03_linear_glmnet,
    model_04_tree_decision_tree,
    model_05_rand_forest_ranger,
    model_06_rand_forest_randomForest,
    model_07_boost_tree_xgboost
  )
)

models_tbl

# Add Predictions

predictions_new_cross_country_tbl <- models_tbl %>%
  mutate(predictions = map(model, predict, new_data = new_cross_country)) %>%
  unnest(predictions) %>%
  mutate(category_2 = "Cross-Country") %>%
  left_join(new_cross_country, by = "category_2")

predictions_new_cross_country_tbl

# Update plot
g2 <- g1 +
  geom_point(aes(y = .pred), color = "red", alpha = 0.5,
             data = predictions_new_cross_country_tbl) +
  ggrepel::geom_text_repel(aes(label = model_id, y = .pred),
                           size = 3,
                           data = predictions_new_cross_country_tbl)


# Challenge

# Load required libraries
library(tidymodels)
library(broom.mixed)
library(ggplot2)

# Load the dataset
bike_data_tbl <- readRDS("scripts/data/bike_orderlines.rds")

# Split the data into training and testing sets
set.seed(123)
data_split <- initial_split(bike_data_tbl, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create the recipe (without prep)
recipe_obj <- recipe(weight ~ price + category_1, data = train_data) %>% 
  step_dummy(all_nominal(), one_hot = TRUE) %>% 
  step_corr(all_predictors(), threshold = 0.9)

# Define the linear regression model
lm_mod <- linear_reg() %>% 
  set_engine("lm")

# Create a workflow
bike_workflow <- workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(recipe_obj)

# Train the workflow
bike_workflow_fit <- bike_workflow %>% 
  fit(data = train_data)

# Make predictions on the test set
test_predictions <- bike_workflow_fit %>% 
  predict(new_data = test_data) %>% 
  bind_cols(test_data)

# Evaluate the model
metrics <- test_predictions %>% 
  metrics(truth = weight, estimate = .pred)

# Print the metrics
print(metrics)

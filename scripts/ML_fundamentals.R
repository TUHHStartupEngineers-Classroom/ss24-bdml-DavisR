library(tidyverse)
library(modelr)
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "#2DC6D6")


models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  ) +
  scale_color_continuous(low = "#2097A3", high = "#95E1EA")

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) +
  scale_color_continuous(low = "#2097A3", high = "#95E1EA")

grid <- expand_grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))


ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
## [1] 4.22 2.05

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])


sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)


# K-Means
library(readxl)
library(tidyverse)
library(broom)
library(umap)
library(ggrepel) # Addon for ggplot, so that the labels do not overlap

stores_tbl      <- read_excel(path  = "scripts/data/breakfast_at_the_frat.xlsx", 
                              sheet = "dh Store Lookup",
                              skip  = 1)
products_tbl    <- read_excel(path  = "scripts/data/breakfast_at_the_frat.xlsx", 
                              sheet = "dh Products Lookup",
                              skip  = 1)
transaction_tbl <- read_excel(path  = "scripts/data/breakfast_at_the_frat.xlsx",
                              sheet = "dh Transaction Data", 
                              skip  = 1)

orderlines_tbl <- transaction_tbl %>% 
  left_join(products_tbl) %>% 
  left_join(stores_tbl, by = c("STORE_NUM" = "STORE_ID"))

#1.1 Get Customer Trends ----
customer_trends_tbl <- orderlines_tbl %>%

mutate(BRANDED = ifelse(MANUFACTURER == "PRIVATE LABEL", "no", "yes")) %>% 
select(STORE_NAME, PRICE, UPC, DESCRIPTION, CATEGORY, SUB_CATEGORY, BRANDED, UNITS) %>%

# Summarization and group by
group_by(STORE_NAME, PRICE, UPC, DESCRIPTION, CATEGORY, SUB_CATEGORY, BRANDED) %>%
summarise(QUANTITY_PURCHASED = sum(UNITS)) %>%
ungroup() %>%

# Proportion
group_by(STORE_NAME) %>%
mutate(PROP_OF_TOTAL = QUANTITY_PURCHASED / sum(QUANTITY_PURCHASED)) %>%
ungroup()

# 1.2 Convert to User-Item Format (e.g. Customer-Product) ----

customer_product_tbl <- customer_trends_tbl %>%

select(STORE_NAME, UPC, PROP_OF_TOTAL) %>%
pivot_wider(names_from = UPC, values_from = PROP_OF_TOTAL, values_fill = 0) %>%
ungroup()

# 2.0 MODELING: K-MEANS CLUSTERING ----

# 2.1 Performing K-Means ----
?kmeans

kmeans_obj <- customer_product_tbl %>%
  select(-STORE_NAME) %>%
  kmeans(centers = 3, nstart = 100)

# The group that each product is assigned to for each product   
kmeans_obj$cluster


# 2.2 Tidying a K-Means Object ----
# return the centers information for the kmeans model
broom::tidy(kmeans_obj) %>% glimpse()

# return the overall summary metrics for the model
# Including the tot.withinss for the skree plot
broom::glance(kmeans_obj)

# Add the clusters to the data
broom::augment(kmeans_obj, customer_product_tbl) %>%
  select(STORE_NAME, .cluster)


# 2.3 How many centers (customer groups) to use? ----

# Functions that works on 1 element
# Setting default centers to 3
kmeans_mapper <- function(centers = 3) {
  
  customer_product_tbl %>%
    select(-STORE_NAME) %>%
    kmeans(centers = centers, nstart = 100)
}

3 %>% kmeans_mapper() %>% glance()


# Mapping the function to many elements
kmeans_mapped_tbl <- tibble(centers = 1:15) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance  = k_means %>% map(glance))

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss)


# 2.4 Skree Plot ----

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  
  # Visualization
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  # Add labels (which are repelled a little)
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") + 
  
  # Formatting
  labs(title = "Skree Plot",
       subtitle = "Measures the distance each of the customer are from the closes K-Means center",
       caption = "Conclusion: Based on the Scree Plot, we select 3 clusters to segment the customer base.")


# 3.0 VISUALIZATION: UMAP ----

# 3.1 Use UMAP to get 2-D Projection ----
?umap

umap_obj <- customer_product_tbl %>%
  select(-STORE_NAME) %>%
  umap()

umap_results_tbl <- umap_obj$layout %>%
  as_tibble(.name_repair = "unique") %>% # argument is required to set names in the next step
  set_names(c("x", "y")) %>%
  bind_cols(
    customer_product_tbl %>% select(STORE_NAME)
  )

umap_results_tbl %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_label_repel(aes(label = STORE_NAME), size = 3)

# 3.2 Use K-Means to Add Cluster Assignments ----
umap_results_tbl

# Get the data for the third element (which we have chosen in the skree plot)
kmeans_3_obj <- kmeans_mapped_tbl %>%
  pull(k_means) %>%
  pluck(3)

# Convert it to a tibble with broom
kmeans_3_clusters_tbl <- kmeans_3_obj %>% 
  augment(customer_product_tbl) %>%
  # Select the data we need
  select(STORE_NAME, .cluster)

# Bind data together
umap_kmeans_3_results_tbl <- umap_results_tbl %>%
  left_join(kmeans_3_clusters_tbl)

# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----

umap_kmeans_3_results_tbl %>%
  mutate(label_text = str_glue("Customer: {STORE_NAME}
                                 Cluster: {.cluster}")) %>%
  
  ggplot(aes(x, y, color = .cluster)) +
  
  # Geometries
  geom_point() +
  geom_label_repel(aes(label = label_text), size = 2, fill = "#282A36") +
  
  # Formatting
  scale_color_manual(values=c("#2d72d6", "#2dc6d6", "#2dd692")) +
  labs(title = "Customer Segmentation: 2D Projection",
       subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
       caption = "Conclusion: 3 Customer Segments identified using 2 algorithms") +
  theme(legend.position = "none")


# 4.0 ANALYZE PURCHASING TRENDS ----

# pipe part 1
cluster_trends_tbl <- customer_trends_tbl %>%
  
  # Join Cluster Assignment by store name
  left_join(umap_kmeans_3_results_tbl) %>%
  ### continued in pipe part 2 ###
  
  
  
?quantile
customer_trends_tbl %>%
  pull(PRICE) %>%
  quantile(probs = c(0, 0.33, 0.66, 1))
##   0%  33%  66% 100% 
## 1.30 2.77 4.05 7.50 

# pipe part 2
mutate(price_bin = case_when(
  PRICE <= 2.77 ~ "low",
  PRICE <= 4.05 ~ "medium",
  TRUE ~ "high"
))  %>%
  # pipe part 3
  select(.cluster, UPC, DESCRIPTION, contains("PRICE"), 
         CATEGORY:QUANTITY_PURCHASED) %>%
  # pipe part 4    
  # Aggregate quantity purchased by cluster and product attributes
  group_by_at(.vars = vars(.cluster:BRANDED)) %>%
  summarise(TOTAL_QUANTITY = sum(QUANTITY_PURCHASED)) %>%
  ungroup() %>%
  
  # pipe part 5  
  # Calculate Proportion of Total
  group_by(.cluster) %>%
  mutate(PROP_OF_TOTAL = TOTAL_QUANTITY / sum(TOTAL_QUANTITY)) %>%
  ungroup()

cluster_trends_tbl
  

# Cluster 1
cluster_trends_tbl %>%
  filter(.cluster == 1) %>%
  arrange(desc(PROP_OF_TOTAL)) %>%
  mutate(CUM_PROP = cumsum(PROP_OF_TOTAL)) %>%
  View()

# Create function and do it for each cluster
get_cluster_trends <- function(cluster = 1) {
  
  cluster_trends_tbl %>%
    filter(.cluster == cluster) %>%
    arrange(desc(PROP_OF_TOTAL)) %>%
    mutate(cum_prop = cumsum(PROP_OF_TOTAL)) 
  
}

get_cluster_trends(cluster = 1)
get_cluster_trends(cluster = 2)
get_cluster_trends(cluster = 3)












# Challenge
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)

# Step 1
sp_500_daily_returns_tbl <- readRDS("scripts/data/sp_500_prices_tbl.rds")

# Perform the operations
sp_500_daily_returns_tbl <- sp_500_daily_returns_tbl %>%
  # Select the necessary columns
  select(symbol, date, adjusted) %>%
  # Filter the data to include only dates from 2018 and beyond
  filter(as.Date(date) >= as.Date("2018-01-01")) %>%
  # Group by symbol
  group_by(symbol) %>%
  # Compute the lag of 1 day on the adjusted stock price
  mutate(lag_adjusted = lag(adjusted, 1)) %>%
  # Remove NA values from the lagging operation
  filter(!is.na(lag_adjusted)) %>%
  # Compute the difference between adjusted and the lag
  mutate(diff = adjusted - lag_adjusted) %>%
  # Compute the percentage difference
  mutate(pct_return = (diff / lag_adjusted)) %>%
  # Select only the symbol, date, and pct_return columns
  select(symbol, date, pct_return) %>%
  # Sort by symbol
  arrange(symbol)


# Display the resulting data frame
sp_500_daily_returns_tbl

# Step 2
# Convert the data frame to the desired format
stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>%
  # Spread the date column to wide format with pct_return as values
  pivot_wider(names_from = date, values_from = pct_return, values_fill = list(pct_return = 0)) %>%
  ungroup

# Display the resulting data frame
stock_date_matrix_tbl

# Step 3
# Impute missing values with column means and convert to user-item format
stock_date_matrix_numeric_imputed <- stock_date_matrix_tbl %>%
  select(-symbol) 
  #%>% pivot_wider(names_from = date, values_from = pct_return, values_fill = mean, values_fn = list(pct_return = mean))

# Perform K-Means clustering with centers = 4 and nstart = 20
set.seed(123) # Setting a seed for reproducibility
kmeans_obj <- kmeans(stock_date_matrix_numeric_imputed, centers = 4, nstart = 20)

# Use glance() to get the tot.withinss
kmeans_summary <- glance(kmeans_obj)

# Print the tot.withinss
print("Total within-cluster sum of squares (tot.withinss):")
print(kmeans_summary$tot.withinss)


# Step 4
kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}


# Create a tibble with column centers from 1 to 30
centers_tbl <- tibble(centers = 1:30)

# Add a column named k_means with the kmeans_mapper() output
k_means_mapped_tbl <- centers_tbl %>%
  mutate(k_means = map(centers, ~kmeans_mapper(.x)))

# Add a column named glance with the glance() output
k_means_mapped_tbl <- k_means_mapped_tbl %>%
  mutate(glance = map(k_means, glance))

# Unnest the glance column
k_means_mapped_tbl <- k_means_mapped_tbl %>%
  unnest(glance)

# Visualize Scree Plot
ggplot(k_means_mapped_tbl, aes(x = centers, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  labs(title = "Scree Plot") +
  theme_minimal()


# Step 5
# Apply UMAP to stock_date_matrix_tbl
umap_results <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  umap()

# Store UMAP results
umap_results_tbl <- umap_results$layout %>%
  as_tibble() %>%
  bind_cols(symbol = stock_date_matrix_tbl$symbol)

# Visualize UMAP results
umap_results_tbl %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point(alpha = 0.5) +
  theme_tq() +
  labs(title = "UMAP Projection")



# Step 6
sp_500_index_tbl <- read_rds("scripts/data/sp_500_index_tbl.rds")
k_means_mapped_tbl <- read_rds("scripts/data/k_means_mapped_tbl.rds")
umap_results_tbl   <- read_rds("scripts/data/umap_results_tbl.rds")
stock_date_matrix_tbl <- read_rds("scripts/data/stock_date_matrix_tbl.rds")

# Get the k_means_obj from the 10th center
k_means_obj <- k_means_mapped_tbl %>%
  filter(centers == 10) %>%
  pull(k_means) %>%
  pluck(1)

# Extract the cluster assignments from k_means_obj
k_means_clusters <- k_means_obj$cluster

# Augment the k_means_obj with the stock_date_matrix_tbl
# Add the clusters to the data
umap_kmeans_results_tbl <- broom::augment(k_means_obj, stock_date_matrix_tbl) %>%
  select(symbol , .cluster) %>%
  left_join(umap_results_tbl, by = "symbol") %>%
  left_join(sp_500_index_tbl %>% select(symbol, company, sector), by = "symbol")


# Plot the K-Means and UMAP results
ggplot(umap_kmeans_results_tbl, aes(V1, V2, color = .cluster)) +
  geom_point(alpha = 0.5) 
#+ scale_color_manual(values = palette_light() %>% rep(3))

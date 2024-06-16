library(h2o)
library(tidyverse)

# To launch H2O locally with default initialization arguments, use the following: 
h2o.init()

hp_by_cyl <- mtcars %>% 
  group_by(cyl) %>%
  summarize(min_hp=min(hp),
            max_hp=max(hp))
hp_by_cyl


groupby_var <- quo(vs)

hp_by_vs <- mtcars %>% 
  group_by(!!groupby_var) %>%
  summarize(min_hp=min(hp),
            max_hp=max(hp))
hp_by_vs

car_stats <- function(groupby_var, measure_var) {
  
  groupby_var <- enquo(groupby_var)
  measure_var <- enquo(measure_var)
  
  ret <- mtcars %>% 
    
    group_by(!!groupby_var) %>%
    summarize(min = min(!!measure_var), max = max(!!measure_var)) %>%
    
    # Optional: as_label() and "walrus operator" :=
    mutate(
      measure_var = as_label(measure_var), !!measure_var := "test"
    )
  
  return(ret)
  
}

car_stats(am,hp)
car_stats(gear,cyl)


scatter_plot <- function(data, x_var, y_var) {
  
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  
  ret <- data %>% 
    ggplot(aes(x = !!x_var, y = !!y_var)) + 
    geom_point() + 
    geom_smooth() +
    ggtitle(str_c(as_label(y_var), " vs. ",as_label(x_var)))
  
  return(ret)
}
scatter_plot(mtcars, disp, hp)


# Load data
employee_attrition_tbl <- read_csv("scripts/data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Business & Data Understanding: Department and Job Role

# Data subset
dept_job_role_tbl <- employee_attrition_tbl %>%
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

dept_job_role_tbl %>%
  
  group_by(Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n))


# Attrition by department
dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2: Caution: It's easy to inadvertently miss grouping when creating counts & percents within groups
  group_by(Department) %>%
  mutate(pct = n / sum(n))


# Attrition by job role
dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes")



# Develop KPI

dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  )


# Function to calculate attrition cost
calculate_attrition_cost <- function(
    
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
  
) {
  
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year *
    (workdays_position_open + workdays_onboarding * onboarding_efficiency)
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
}

calculate_attrition_cost()
calculate_attrition_cost(200)

dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(
    above_industry_avg = case_when(
      pct > 0.088 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  
  # Block 4. Set salaray to 80000 for now
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  )

# Use this
dept_job_role_tbl %>%
  
  count(Department, JobRole, Attrition)

# Use this
# Function to convert counts to percentages. 
count_to_pct <- function(data, ..., col = n) {
  
  # capture the dots
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()
  
  return(ret)
  
}

# This is way shorter and more flexibel
dept_job_role_tbl %>%
  count(JobRole, Attrition) %>%
  count_to_pct(JobRole)

dept_job_role_tbl %>%
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole)  

assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
  
  attrition_col_expr <- enquo(attrition_col)
  
  data %>%
    
    # Use parenthesis () to give tidy eval evaluation priority
    filter((!! attrition_col_expr) %in% attrition_value) %>%
    arrange(desc(pct)) %>%
    mutate(
      # Function inputs in numeric format (e.g. baseline_pct = 0.088 don't require tidy eval)
      above_industry_avg = case_when(
        pct > baseline_pct ~ "Yes",
        TRUE ~ "No"
      )
    )
  
}

#source("assess_attrition.R")
dept_job_role_tbl %>%
  
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  )


dept_job_role_tbl %>%
  
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>%
  
  # Data Manipulation
  mutate(name = str_c(Department, JobRole, sep = ": ") %>% as_factor()) %>%
  
  # Check levels
  # pull(name) %>%
  # levels()
  
  mutate(name      = fct_reorder(name, cost_of_attrition)) %>%
  mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6, digits = 2),
                           "M", sep = "")) %>%
  
  #Plotting
  ggplot(aes(cost_of_attrition, y = name)) +
  geom_segment(aes(xend = 0, yend = name),    color = "#2dc6d6") +
  geom_point(  aes(size = cost_of_attrition), color = "#2dc6d6") +
  scale_x_continuous(labels = scales::dollar) +
  geom_label(aes(label = cost_text, size = cost_of_attrition),
             hjust = "inward", color = "#2dc6d6") +
  scale_size(range = c(3, 5)) +
  labs(title = "Estimated cost of Attrition: By Dept and Job Role",
       y = "",
       x = "Cost of attrition") +
  theme(legend.position = "none")

# This will return a quoted result
colnames(dept_job_role_tbl)[[1]]
## "EmployeeNumber"

# This will become an unquoted expression
rlang::sym(colnames(dept_job_role_tbl)[[1]])
## EmployeeNumber

# quos() captures it and turns it into a quosure, which is a list
# Will be evaluated at the time we use the double !! later on in the code.
# Then it will turn it into EmployeeNumber
quos(rlang::sym(colnames(employee_attrition_tbl)[[1]]))
## <list_of<quosure>>
##
## [[1]]
## <quosure>
## expr: ^rlang::sym(colnames(employee_attrition_tbl)[[1]])
## env:  global

# If the user supplies two different columns such as Department and Job Role
# or if the user does not supply a column the length will be different
quos(Department, JobRole) 
quos(Department, JobRole) %>% length()
## 2
quos() %>% length

# Function to plot attrition
plot_attrition <- function(data, 
                           ..., 
                           .value,
                           fct_reorder = TRUE,
                           fct_rev     = FALSE,
                           include_lbl = TRUE,
                           color       = "#2dc6d6",
                           units       = c("0", "K", "M")) {
  
  ### Inputs
  group_vars_expr   <- quos(...)
  
  # If the user does not supply anything, 
  # this takes the first column of the supplied data
  if (length(group_vars_expr) == 0) {
    group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
  }
  
  value_expr <- enquo(.value)
  
  units_val  <- switch(units[[1]],
                       "M" = 1e6,
                       "K" = 1e3,
                       "0" = 1)
  if (units[[1]] == "0") units <- ""
  
  # Data Manipulation
  # This is a so called Function Factory (a function that produces a function)
  usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)
  
  # Create the axis labels and values for the plot
  data_manipulated <- data %>%
    mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>%
    mutate(value_text = str_c(usd(!! value_expr / units_val),
                              units[[1]], sep = ""))
  
  
  # Order the labels on the y-axis according to the input
  if (fct_reorder) {
    data_manipulated <- data_manipulated %>%
      mutate(name = forcats::fct_reorder(name, !! value_expr)) %>%
      arrange(name)
  }
  
  if (fct_rev) {
    data_manipulated <- data_manipulated %>%
      mutate(name = forcats::fct_rev(name)) %>%
      arrange(name)
  }
  
  # Visualization
  g <- data_manipulated %>%
    
    # "name" is a column name generated by our function internally as part of the data manipulation task
    ggplot(aes(x = (!! value_expr), y = name)) +
    geom_segment(aes(xend = 0, yend = name), color = color) +
    geom_point(aes(size = !! value_expr), color = color) +
    scale_x_continuous(labels = scales::dollar) +
    scale_size(range = c(3, 5)) +
    theme(legend.position = "none")
  
  # Plot labels if TRUE
  if (include_lbl) {
    g <- g +
      geom_label(aes(label = value_text, size = !! value_expr),
                 hjust = "inward", color = color)
  }
  
  return(g)
  
}


dept_job_role_tbl %>%
  
  # Select columnns
  count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n, salary = 80000)
  ) %>%
  
  # Select columnns
  plot_attrition(Department, JobRole, .value = cost_of_attrition,
                 units = "M") +
  labs(
    title = "Estimated Cost of Attrition by Job Role",
    x = "Cost of Attrition",
    subtitle = "Looks like Sales Executive and Labaratory Technician are the biggest drivers of cost"
  )


# Libraries 
library(tidyverse)
library(readxl)
library(skimr)
library(GGally)

# Load Data data definitions

path_data_definitions <- "scripts/data/data_definitions.xlsx"
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

employee_attrition_tbl

# Descriptive Features
employee_attrition_tbl %>% select(Age, DistanceFromHome, Gender, MaritalStatus, NumCompaniesWorked, Over18)

# Employment Features
employee_attrition_tbl %>% select(Department, EmployeeCount, EmployeeNumber, JobInvolvement, JobLevel, JobRole, JobSatisfaction)

# Compensation Features
employee_attrition_tbl %>% select(DailyRate, HourlyRate, MonthlyIncome, MonthlyRate, PercentSalaryHike, StockOptionLevel)

# Survery Results
employee_attrition_tbl %>% select(EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance)

# Performance Data
employee_attrition_tbl %>% select(JobInvolvement, PerformanceRating)

# Work-Life Features
employee_attrition_tbl %>% select(BusinessTravel, OverTime)

# Training & Education
employee_attrition_tbl %>% select(Education, EducationField, TrainingTimesLastYear)

# Time-Based Features
employee_attrition_tbl %>% select(TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)
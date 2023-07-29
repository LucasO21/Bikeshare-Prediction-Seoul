# SEOUL BIKE SHARING DEMAND PREDICTION ----
# ANALYSIS & MODELING SCRIPT ----
# Data Source: # "https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv"
# **** ----

# ******************************************************************************
# 1.0: Setup ----
# ******************************************************************************

# * Set Working Directory ----
setwd(here::here("scripts"))

# * Libraries ----
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(plotly)
library(DT)
library(kableExtra)
library(tidymodels)
library(rules)
library(vip)
library(tictoc)
library(future)
library(doFuture)
library(parallel)

# * Load Dataset ----
colnames <-  c("date", "rented_count", "hour", "temp", "humidity", "windspeed", "visibility", 
               "dew_point", "solar_rad", "rainfall", "snowfall", "season", "holiday", "functional_day")

seoul_bikes_raw_tbl <- read.csv("../data/SeoulBikeData.csv", header = F, skip = 1) %>% 
    as_tibble() %>% 
    setNames(colnames) %>% 
    clean_names()


# * Data Inspection ----
seoul_bikes_raw_tbl %>% glimpse()

dim(seoul_bikes_raw_tbl)

seoul_bikes_raw_tbl %>% sapply(function(x)sum(is.na(x)))


# ******************************************************************************
# **** ----
# 2.0: Exploratory Data Analysis ----
# ******************************************************************************

# * Function ----
get_ggplot_custom_theme <- function() {

        theme(
            plot.title = element_text(size = 18),
            plot.subtitle = element_text(size = 15),
            axis.text = element_text(size = 9),
            axis.title = element_text(size = 9)
                
        )
}

# * Numeric Variables ----
seoul_bikes_raw_tbl %>% 
    select_if(is.numeric) %>% 
    head()

# * Categorical Variables ----
seoul_bikes_raw_tbl %>% 
    select_if(is.character) %>% 
    head()

# * Skim ----
skim(seoul_bikes_raw_tbl)

# * Format Data ----
seoul_bikes_tbl <- seoul_bikes_raw_tbl %>% 
    
    # # add date features
    mutate(date = dmy(date)) %>% 
    mutate(day_of_week = wday(date, label = TRUE) %>% as.factor) %>% 
    mutate(month = month(date, label = TRUE) %>% as.factor) %>% 
    mutate_if(is.character, as.factor) %>% 
    
    # filter function_day = "yes"
    filter(functional_day == "Yes")
    
# * Overall Distribution of Rented Count ----
seoul_bikes_tbl %>% 
    ggplot(aes(rented_count))+
    geom_histogram(color = "grey30", fill = "#00bfc4", binwidth = 50)+
    theme_bw()+
    get_ggplot_custom_theme()
    labs(
        title = "Distribution of Rented Count",
         x    = "Rented Count",
         y    = "Frequency"
    )+
    theme_bw()+
    get_ggplot_custom_theme()
    
# * Hourly Distribution of Rented Count ----
seoul_bikes_tbl %>% 
    mutate(hour = as.factor(hour)) %>% 
    ggplot(aes(hour,rented_count))+
    geom_boxplot(color = "grey30", fill = "#00bfc4", outlier.colour = "red")+
    # scale_x_continuous(limits = c(0, 23), breaks = seq(0, 23, by = 1))+
    theme_bw()+
    get_ggplot_custom_theme()
    labs(
        title = "Bike Rentals by Hour of Day",
         x    = "Hour of Day",
         y    = "Rented Count"
    )

# * Daily Distribution of Rented Count ----
seoul_bikes_tbl %>% 
    ggplot(aes(day_of_week, rented_count))+
    geom_boxplot(color = "grey30", fill = "#00bfc4", outlier.colour = "red")+
    theme_bw()+
    get_ggplot_custom_theme()+
    labs(
        title    = "Daily Rented Count",
        subtitle = "More rentals on Mondays, Wednesdays & Fridays",
        y        = "Rented Count",
        x        = "Day of Week"
    )
    

# * Hourly Rental Distribution by Season ----
seoul_bikes_tbl %>% 
    mutate(hour = as.factor(hour)) %>% 
    ggplot(aes(hour, rented_count))+
    geom_boxplot(color = "grey30", fill = "#00bfc4", outlier.colour = "red")+
    facet_wrap(~ season, scales = "free")+
    theme_bw()+
    get_ggplot_custom_theme()+
    labs(
        title = "Bike Rentals by Hour of Day",
        x     = "Hour",
        y     = "Rental Count"
    )
  

# * Daily Rental Distribution by Season ----
seoul_bikes_tbl %>% 
    mutate(hour = as.factor(hour)) %>% 
    ggplot(aes(day_of_week,rented_count))+
    geom_boxplot(color = "grey30", fill = "#00bfc4", outlier.colour = "red")+
    facet_wrap(~ season, scales = "free")+
    theme_bw()+
    get_ggplot_custom_theme()+
    labs(
        title = "Bike Rentals by Hour of Day",
        x     = "Hour",
        y     = "Rental Count"
    )
    
  
# * Rented Count vs Temp / Season ----
seoul_bikes_tbl %>% 
    ggplot(aes(temp, rented_count, color = season))+
    geom_point(alpha = 0.5)+
    theme_bw()+
    get_ggplot_custom_theme()+
    labs(title = "Rented Count vs Temp", y = "Rented Count", x = "Temp")

    
# *  Correlation Matrix ----
cor_matrix <- seoul_bikes_tbl %>% 
    select_if(is.numeric) %>% 
    cor() %>% 
    round(2) %>% 
    reshape2::melt()

cor_matrix %>% 
    ggplot(aes(Var1, Var2, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(
        low = "#67f8fd", high = "#00898e", mid = "white",
        midpoint = 0, limit = c(-1, 1), space = "lab"
    )+
    theme_bw()+
    get_ggplot_custom_theme()+
    geom_text(aes(label = value))+
    labs(
        title = "Correlation Heatmap",
        x = NULL, y = NULL
    )+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))


# * Distribution of Other Numeric Weather Features ----
seoul_bikes_tbl %>% 
    select_if(is.numeric) %>% 
    select(-rented_count, -hour) %>% 
    gather() %>% 
    ggplot(aes(value))+
    geom_histogram(color = "white", fill = "#00bfc4")+
    theme_bw()+
    get_ggplot_custom_theme()+
    facet_wrap(~ key, scales = "free")
    
    
# ******************************************************************************
# **** ----
# 3.0: Modeling ----
# ******************************************************************************

# * 3.1: Data Splitting ----
set.seed(100)
split_obj <- initial_split(seoul_bikes_tbl, prop = 0.80, strata = rented_count)

train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

# * 3.2: Cross Validation Specs ----
set.seed(101)
resamples_obj <- vfold_cv(seoul_bikes_tbl, v = 10)


# * 3.3: Recipes ----

# Random Forest Recipe Spec
recipe_spec_ranger <- recipe(rented_count ~ ., data = train_tbl) %>% 
    step_mutate(am_pm = ifelse(hour < 12, "am", "pm")) %>% 
    timetk::step_timeseries_signature(date) %>% 
    step_rm(
        matches("(.xts)|(.iso)|(date_hour)|(minute)|(second)"),
        day_of_week, month, date_index.num, functional_day, date
    ) %>% 
    step_zv(all_predictors())

recipe_spec_ranger %>% prep() %>% juice() %>% glimpse()


# Xgboost Recipe Spec
recipe_spec_xgboost <- recipe(formula = rented_count ~ ., data = seoul_bikes_tbl) %>% 
    step_mutate(am_pm = ifelse(hour < 12, "am", "pm")) %>% 
    timetk::step_timeseries_signature(date) %>% 
    step_rm(
        matches("(.xts)|(.iso)|(date_hour)|(minute)|(second)"),
        day_of_week, month, date_index.num, functional_day, date
    ) %>% 
    step_novel(all_nominal(), -all_outcomes()) %>% 
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% 
    step_zv(all_predictors())

recipe_spec_xgboost %>% prep() %>% juice() %>% glimpse()


# Cubist Recipe Spec
recipe_spec_cubist <- recipe(formula = rented_count ~ ., data = seoul_bikes_tbl) %>% 
    step_mutate(am_pm = ifelse(hour < 12, "am", "pm")) %>% 
    timetk::step_timeseries_signature(date) %>% 
    step_rm(
        matches("(.xts)|(.iso)|(date_hour)|(minute)|(second)"),
        day_of_week, month, date_index.num, functional_day, date
    ) %>% 
    step_zv(all_predictors())

recipe_spec_cubist %>% prep() %>% juice() %>% glimpse()


# * 3.4: Model Specs ----

# Random Forest Model Spec
model_spec_ranger <- rand_forest(
    mtry  = tune(),
    min_n = tune(),
    trees = 1000
) %>%
    set_mode("regression") %>%
    set_engine("ranger") 

# Xgboost Model Spec
model_spec_xgboost <-boost_tree(
    trees          = tune(),
    min_n          = tune(),
    tree_depth     = tune(),
    learn_rate     = tune(),
    loss_reduction = tune(),
    sample_size    = tune()
) %>%
    set_mode("regression") %>%
    set_engine("xgboost") 

# Cubist Model Spec
model_spec_cubist <- cubist_rules(
    committees = tune(), 
    neighbors  = tune()
) %>%
    set_engine("Cubist") 


# * 3.1: Workflows ----

# Random Forest Workflow
wflw_ranger <- 
    workflow() %>% 
    add_recipe(recipe_spec_ranger) %>% 
    add_model(model_spec_ranger) 

# Xgboost Workflow
wflw_xgboost <- 
    workflow() %>% 
    add_recipe(recipe_spec_xgboost) %>% 
    add_model(model_spec_xgboost) 

# Cubist Workflow
wflw_cubist <- 
    workflow() %>% 
    add_recipe(recipe_spec_cubist) %>% 
    add_model(model_spec_cubist) 


# 4.0: Hyper-Parameter Tuning Round 1 ----

# * 4.1: Setup Parallel Processing ----
registerDoFuture()
n_cores <- detectCores()
plan(strategy = cluster, workers = makeCluster(n_cores))

# * 4.2: Random Forest Tuning ----
tic()
set.seed(123)
tune_results_ranger_1 <- tune_grid(
    object    = wflw_ranger, 
    resamples = resamples_obj,
    grid      = grid_latin_hypercube(
                        parameters(model_spec_ranger) %>% 
                            update(mtry = mtry(range = c(1, 30))),
                        size = 15),
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()

# Random Forest Round 1 Results
tune_results_ranger_1 %>% show_best("mae", n = 5)

# Save Model For Future Use
write_rds(tune_results_ranger_1, file = "../artifacts/tune_results_ranger_1.rds")


# * 4.3: Xgboost Tuning Round 1----
tic()
set.seed(456)
tune_results_xgboost_1 <- tune_grid(
    object    = wflw_xgboost, 
    resamples = resamples_obj,
    grid      = grid_latin_hypercube(parameters(model_spec_xgboost),
                                     size = 15),
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()

# Xgboost Round 1 Results
tune_results_xgboost_1 %>% show_best("rmse", n = 5)

# Save Model For Future Use
write_rds(tune_results_xgboost_1, file = "../artifacts/tune_results_xgboost_1.rds")


# * 4.3: Cubist Tuning Round 1 ----
tic()
set.seed(456)
tune_results_cubist_1 <- tune_grid(
    object    = wflw_cubist, 
    resamples = resamples_obj,
    grid      = grid_latin_hypercube(parameters(model_spec_cubist),
                                     size = 15),
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()


# Cubist Round 1 Results
tune_results_cubist_1 %>% show_best("rmse", n = 5)

# Save Model For Future Use
write_rds(tune_results_cubist_1, file = "../artifacts/tune_results_cubist_1.rds")


# ******************************************************************************
# Reloading Saved Models

## This section is necessary if you have to close your project and resume later.
## Notice that each model was saved to an "artifacts" folder as a .rds object.
## This is to be able to reload and reuse the models in the future.
## In this section, I'm just reloading the saved models.

# Loading Saved Models
tune_results_ranger_1  <- read_rds("../artifacts/tune_results_ranger_1.rds")
tune_results_xgboost_1 <- read_rds("../artifacts/tune_results_xgboost_1.rds")
tune_results_cubist_1  <- read_rds("../artifacts/tune_results_cubist_1.rds")

# ******************************************************************************


# * 4.4: Training Results Metrics Comparison ----

# Function To Get Model Model Metrics
get_best_metric <- function(model, model_name){
    
    bind_rows(
        model %>% show_best("mae", 1),
        model %>% show_best("rmse", 1),
        model %>% show_best("rsq", 1) 
    ) %>% 
        mutate(mean = round(mean, 2)) %>% 
        select(.metric, mean) %>% 
        spread(key = .metric, value = mean) %>% 
        mutate(model := {{model_name}}) %>% 
        select(model, everything(.))
}

ranger_metrics_1  <- get_best_metric(tune_results_ranger_1, "Random Forest")
xgboost_metrics_1 <- get_best_metric(tune_results_xgboost_1, "Xgboost")
cubist_metrics_1  <- get_best_metric(tune_results_cubist_1, "Cubist")

# Training Set Metrics Table
training_metrics_1 <- bind_rows(
    ranger_metrics_1,
    xgboost_metrics_1,
    cubist_metrics_1
) %>% 
    arrange(rmse) %>% 
    kable("html", table.attr = "style='width: 800px;'") %>%
    kable_styling(full_width = FALSE) %>% 
    column_spec(1, width = "270px")
    

training_metrics_1


# ******************************************************************************
# **** ----
# 5.0: Hyper-Parameter Tuning Round 2 ----
# ******************************************************************************

# 5.1: XGBOOST Tuning Round 2 ----

# 5.1.1: Visualize XGBOOST Tuning Params ----
p <- tune_results_xgboost_1 %>% 
    autoplot()+
    theme_bw()+
    get_ggplot_custom_theme()+
    labs(title = "XGBOOST Tuning Parameters")

p


# 5.1.2: Updated XGBOOST Grid ----
set.seed(123)
grid_spec_xgboost_round_2 <- grid_latin_hypercube(
    parameters(model_spec_xgboost) %>% 
        update(
            trees = trees(range = c(1000, 2000)),
            learn_rate = learn_rate(range = c(-2.0, -1.0)),
            min_n = min_n(range = c(20, 30))
        ),
    size = 15
)

# 5.1.3: Tuning Round 2 ----
tic()
set.seed(654)
xgboost_tune_results_2 <- tune_grid(
    object    = wflw_xgboost, 
    resamples = resamples_obj,
    grid      = grid_spec_xgboost_round_2,
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()

# 5.1.4: Xgboost Round 2 Results ----
xgboost_tune_results_2 %>% show_best("rmse", n = 5)

# Save Model For Future Use
write_rds(xgboost_tune_results_2, file = "../artifacts/tune_results_xgboost_2.rds")


# 5.2: Cubist Tuning Round 2 ----

# 5.2.1: Visualize Cubist Tuning Params
p <- tune_results_cubist_1 %>% 
    autoplot()+
    theme_bw()+
    get_ggplot_custom_theme()+
    labs(title = "Cubist Tuning Parameters")

p

# 5.2.2: Updated Cubist Grid ----
set.seed(123)
grid_spec_cubist_round_2 <- grid_latin_hypercube(
    parameters(model_spec_cubist) %>% 
        update(
            committees = committees(range = c(75, 100)),
            neighbors = neighbors(range = c(1, 3))),
    size = 10
)

# 5.2.3: Tuning Round 2 ----
tic()
set.seed(654)
cubist_tune_results_2 <- tune_grid(
    object    = wflw_cubist, 
    resamples = resamples_obj,
    grid      = grid_spec_cubist_round_2,
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = TRUE),
    metrics   = metric_set(mae, rmse, rsq)
)
toc()

# 5.2.4: Cubist Round 2 Results ----
cubist_tune_results_2 %>% show_best("rmse", n = 5)

# Save Model For Future Use
write_rds(cubist_tune_results_2, file = "../artifacts/tune_results_cubist_2.rds")


# ******************************************************************************
# Reloading Saved Models

## This section is necessary if you have to close your project and resume later.
## Notice that each model was saved to an "artifacts" folder as a .rds object.
## This is to be able to reload and reuse the models in the future.
## In this section, I'm just reloading the saved models.

# Loading Saved Models
# tune_results_xgboost_2 <- read_rds("../artifacts/tune_results_xgboost_2.rds")
# tune_results_cubist_2  <- read_rds("../artifacts/tune_results_cubist_2.rds")

# ******************************************************************************

# * 5.3 Training Results Metrics Comparison Round 2 ----
training_metrics_2 <- bind_rows(
  get_best_metric(tune_results_xgboost_2, "Xgboost"),
  get_best_metric(tune_results_cubist_2, "Cubist"),
) %>% 
    arrange(rmse) %>% 
  kable("html", table.attr = "style='width: 800px;'") %>%
  kable_styling(full_width = FALSE) %>% 
  column_spec(1, width = "270px")

training_metrics_2




# 6.0: Finalize Models ----

# * XGBOOST Final Fit ----
xgboost_spec_final <- xgboost_spec %>% 
    finalize_model(parameters = xgboost_tune_results_2 %>% select_best("rmse"))

set.seed(123)
xgboost_last_fit <- workflow() %>% 
    add_model(xgboost_spec_final) %>% 
    add_recipe(xgboost_recipe) %>% 
    last_fit(split_obj, metric_set(mae, rmse, rsq))

# XGBOOST Final Fit (Test Set) Metrics
collect_metrics(xgboost_last_fit)


# * Cubist Final Fit ----
cubist_spec_final <- cubist_spec %>% 
    finalize_model(parameters = cubist_tune_results_2 %>% select_best("rmse"))

set.seed(123)
cubist_last_fit <- workflow() %>% 
    add_model(cubist_spec_final) %>% 
    add_recipe(cubist_recipe) %>% 
    last_fit(split_obj, metric_set(mae, rmse, rsq))

# Cubist Final Fit (Test Set) Metrics
collect_metrics(cubist_last_fit)

# 6.1: Test Results Metrics Comparison ----

# Final Fit (Test) Set Metrics Set (XGBOOST)
xgboost_test_metrics <- collect_metrics(xgboost_last_fit) %>% 
    select(-.config) %>% 
    bind_rows(
        xgboost_last_fit %>% 
            collect_predictions() %>% 
            mae(rented_count, .pred) 
    ) %>% 
    select(-.estimator) %>% 
    mutate(model = "XGBOOST") %>% 
    arrange(.estimate)

# Final Fit (Test) Set Metrics Set (Cubist)
cubist_test_metrics <- collect_metrics(cubist_last_fit) %>% 
    select(-.config) %>% 
    bind_rows(
        cubist_last_fit %>% 
            collect_predictions() %>% 
            mae(rented_count, .pred) 
    ) %>% 
    select(-.estimator) %>% 
    mutate(model = "Cubist") %>% 
    arrange(.estimate)

# Final Fit (Test) Set Metrics Table
test_set_metrics <- bind_rows(
    xgboost_test_metrics,
    cubist_test_metrics
) %>% 
    mutate(.estimate = round(.estimate, 2)) %>% 
    spread(key = .metric, value = .estimate) %>% 
    arrange(mae) %>% 
    datatable(
        class = "cell-border stripe",
        caption = "Test Set Metrics",
        options = list(
            dom = "t"
        )
        
    )


# 7.0: Making Predictions ----

#* To make predictions, we'll use the model offering the best rmse which is the
#* XGBOOST. We'll need to -
#* 1) Train the model on the entire dataset
#* 2) Predict on future data

# * 7.1: Train Model on Entire Data ----
xgboost_model <- xgboost_spec_final %>% 
    fit(rented_count ~ ., data = xgboost_recipe %>% prep %>% bake(new_data = seoul_bikes_tbl))

# * 7.2: Create Sample New Data For Prediction ----
sample_data <- 
    tibble(
        date = ymd("2019-01-24"),
        hour = 6,
        temp = 6,
        humidity = 80, 
        windspeed = 1.8,
        visibility = 1400,
        dew_point = -6.0,
        solar_rad = 0.00,
        rainfall = 0.0,
        snowfall = 0.0,
        season = "Autumn",
        holiday = "No Holiday",
        functional_day = "Yes",
        day_of_week = wday(date, label = TRUE),
        month = month(date, label = TRUE)
    )

sample_data <- sample_data %>% 
    mutate_if(is.character, as.factor)

sample_data

# 7.3: Predictions
xgboost_recipe %>% 
    prep() %>% 
    bake(new_data = sample_data) %>% 
    predict(xgboost_model, new_data = .)

# 8.0: Variable Importance ----
vip_plot <- vip(xgboost_model)+
    theme_bw()+
    labs(title = "XGBOOST Model Variable Importance")

vip_plot

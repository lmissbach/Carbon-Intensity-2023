set.seed(2022)

library(tidymodels)
library(ranger)
library(baguette)
library(randomForest)
library(tidyverse)
library(vip)
library(xgboost)
library(broom)
library(Metrics)

# This script is for experimenting with regression trees / boosted regression trees

data_0 <- read_csv("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/9_Sonstige/Test_Set_Mexico_Israel.csv")

data_0.1 <- data_0 %>%
  filter(Country == "ISR")%>%
  mutate(hardship = ifelse(burden_CO2_national > 0.04,1,0))

data_0.2 <- data_0 %>%
  filter(Country == "MEX")

# 1. Classification trees ####

# Easy to explain and understand.
# Possible to capture non-linear relationships
# Categorical variables, fast for large datasets, robust to outliers

# Sometimes hard to interpret, prone to overfitting

# Pick a model class
# Set the enginge that implements the model
# Set the mode

tree_0 <- decision_tree()%>%
  set_engine("rpart")%>%
  set_mode("classification")

# use classification if outcome is categorical

tree_0.1 <- tree_0 %>%
  fit(formula = factor(hardship) ~ hh_expenditures + factor(ISCED),
      data = data_0.1)

# However, we should split the data.
# Create training and testing sets with training() and testing()
# Training set is used for model build and tuning
# Test set is used for performance evaluation

data_0.1.split <- initial_split(data_0.1, prop = 0.75) 

# argument strata ensures that data is equally distributed

data_0.1.training <- training(data_0.1.split) # returns training set
data_0.1.test     <- testing(data_0.1.split)  # returns test set

rm(data_0.1.split)

# Avoid disembalance

# Train model only with training set

tree_0.2 <- tree_0 %>%
  fit(formula = factor(hardship) ~ hh_expenditures + factor(ISCED),
      data = data_0.1.training)

# Now predict and evaluate

# predicts new data

predictions_0.1 <- predict(tree_0.2, new_data = data_0.1.test,
        type = "class")%>% # also type = "prob" possible
  mutate(true_class = factor(data_0.1.test$hardship))

# Confusion matrix - false and true positives/negatives

conf_mat(data = predictions_0.1,
         estimate = .pred_class,
         truth = true_class)

# Performance: Accuracy

accuracy(data = predictions_0.1,
         estimate = .pred_class,
         truth = true_class)

# 2. Regression trees ####

tree_2 <- decision_tree()%>%
  set_engine("rpart")%>%
  set_mode("regression")

tree_2.1 <- tree_2 %>%
  fit(formula = burden_CO2_national ~ hh_expenditures_USD_2014 + factor(urban_01),
      data = data_0.1.training)

predictions_2.1 <- predict(tree_2.1, new_data = data_0.1.test)

# We want low variance from the mean within groups

# min_n defines the minimum number of data points needed for further split
# tree_depth defines the maximum depth of a tree
# cost_complexity penalizes complexity --> definiton in decision_tree

# Finding optimal parameters is called tuning

# How far are predictions away from the truth?
# Mean absolute error (MAE) - average deviation between true and predicted values
# Root mean square error (RMSE) - root of mean of squared deviation between true and predicted values
# RMSE penalizes large errors more strongly

predictions_2.1 <- predict(tree_2.1, new_data = data_0.1.test)%>%
  rename(bagging_pred = .pred)

# Out-of-sample performance, because we use the test data
# In-sample performance can be assessed with data_0.1.training

mae(predictions_2.1,  estimate = .pred, truth = burden_CO2_national)
rmse(predictions_2.1, estimate = .pred, truth = burden_CO2_national)

# Cross-validation: Average multiple estimates together

# Partion the training dataset into multiple training datasets
# Model is evaluated with every single dataset as a test dataset and all others as test dataset
# Cross-validated out-of-smaple MAE - get a more precise intuition on the performance of the model
# Folds the training set!

# How many folds?
data_0.2.training <- vfold_cv(data_0.1.training, v = 10)
# Fit all folds

tree_2.2 <- fit_resamples(tree_2, # tree specification
                          burden_CO2_national ~ hh_expenditures_USD_2014 + factor(urban_01), # formula
                          resamples = data_0.2.training, # folds
                          metrics   = metric_set(mae, rmse)) # metrics

all_errors   <- collect_metrics(tree_2.2, summarize = FALSE)
all_errors_1 <- collect_metrics(tree_2.2)

# Bias-variance tradeoff
# Hyperparameters: tree_depth, cost_complexity, min_n
# Complex models are prone to overfitting on training set (!) - but high variance when using test set
# Simple models are prone to underfitting on training set (!) - high bias, but low variance when using test set
# There is an optimal complexitiy between overfitting and underfitting

# Compare Out-of-sample and In-Sample MAE

collect_metrics(tree_2.2)
# mae: 0.00802
mae(predictions_2.1,  estimate = .pred, truth = burden_CO2_national)
# mae: 0.00788

# 3. Random forests ####

# Tuning hyperparameters - control a model's complexity
# Default values: min_n = 20 / tree_depth = 30 / cost_complexity = 0.01
# What are the best values for performance? # Use tune()

tree_3 <- decision_tree(min_n      = tune(),
                        tree_depth = tune())%>%
  set_engine("rpart")%>%
  set_mode("regression")

# Create a tuning grid

grid_3 <- grid_regular(parameters(tree_3),
                         levels = 3) # number of grid points for each hyperparameter
# Build a model for every grid point + evaluate every model out-of-sample

tree_3.2 <- tune_grid(tree_3, #untuned specification of tree
                      burden_CO2_national ~ hh_expenditures_USD_2014 + factor(urban_01), # formula
                      resamples = data_0.2.training, # folds
                      grid      = grid_3, # tuning grid
                      metrics   = metric_set(mae))

autoplot(tree_3.2)

# use the best performing parameters

tree_3.3 <- select_best(tree_3.2)

# plug best performing paramters into specification - updated specification

tree_3.4 <- finalize_model(tree_3,
                           tree_3.3)

# For binary classification: 
# Accuracy
# Sensitivity (How many positive outcomes were correctly classified?)
# Specificity (How many negative outcomes were correctly classified?)
# Can also be probabilities --> Receiver-operating-characteristic curve

# e.g.

sens(predictions_0.1, estimate = .pred_class, truth = true_class) # 99.9 % percent of positive outcomes were correctly

predictions_3.1 <- predict(tree_0.2, new_data = data_0.1.test,
                           type = "prob")%>% 
  mutate(true_class = factor(data_0.1.test$hardship))

roc <- roc_curve(predictions_3.1,
                 estimate = .pred_1,
                 truth = true_class)

autoplot(roc)

# Area under curves

auc <- roc_auc(predictions_3.1,)

# Bagged trees - Bootstrap Aggregation
# Bootstrap: Random sampling with replacement --> many modified training sets
# Aggregation: Average
# Utilize several models and their predictions
# Build many models on many samples. Derive predictions from many trees. Aggregate predictions to one ensemble prediction.
# Bagging can reduce the variance

bag_tree_3 <- bag_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart", times = 100)

bag_tree_3.1 <- bag_tree_3 %>%
  fit(formula = factor(hardship) ~ hh_expenditures_USD_2014 + factor(urban_01),
      data = data_0.1.training)

# This takes a little bit more time - e.g. 37.4 s
# Includes importance of variables

data_0.1.training <- data_0.1.training %>%
  mutate(hardship = factor(hardship))

predictions_3.2 <- predict(bag_tree_3.1, new_data = data_0.1.training, type = "prob")%>%
  bind_cols(select(data_0.1.training, hardship))

# Something does not work here.

roc_curve(predictions_3.2, estimate = .pred, truth = hardship)

# Random forests

# Suited for high-dimensional data! 

# Train trees on bootstramp samples - but random (!) predictors (features) across trees
# Subset of variables

rand_3.0 <- rand_forest(mtry  = 3,     # predictors seen at each node --> defaults as square root of predictors
                        trees = 100,   # number of trees in forest
                        min_n = 10)%>% # smallest node size allowed
  set_mode("regression")%>%
  set_engine("ranger", importance = "impurity") # importance can be impurity

data_3.1.training <- data_0.1.training %>%
  select(burden_CO2_national, urban_01, ISCED, Province_code, District_code, hh_expenditures, car.01, Ethnicity_code, religiosity)%>%
  mutate_at(vars(-burden_CO2_national, -hh_expenditures), list(~ factor(.)))

data_3.1.test <- data_0.1.test %>%
  select(burden_CO2_national, urban_01, ISCED, Province_code, District_code, hh_expenditures, car.01, Ethnicity_code, religiosity)%>%
  mutate_at(vars(-burden_CO2_national, -hh_expenditures), list(~ factor(.)))

rand_3.1 <- rand_3.0 %>%
  fit(burden_CO2_national ~ .,
      data = data_3.1.training)

vip(rand_3.1)

predictions_3.3 <- predict(rand_3.1, new_data = data_3.1.test)%>%
  rename(random_pred = .pred)

# 4. Boosted regression trees ####

# Each model is an improvement of its antecedents - iterative learning
# Adaboost: Adaptive boosting - change weights of wrongly classified training examples in subsequent trainings
# Improving by gradient descent - small decision trees instead of weak learners

boost_4 <- boost_tree() %>%
  set_mode("regression")%>%
  set_engine("xgboost")

# Many hyperparameters to be specified !

# min_n, tree_depth
# sample_size, trees, mtry, learn_rate, loss_reduction, stop_iter

# Performance is great among machine learning models - good for unbalanced data
# Prone to overfitting, training can be slow (iterative algorithm), many tuning hyperparameters

boost_4.1 <- boost_4 %>%
  fit(burden_CO2_national ~ .,
      data = data_3.1.training)

folds_4.1 <- vfold_cv(data_3.1.training, v = 10)

folds_4.2 <- fit_resamples(boost_4, 
                           burden_CO2_national ~ .,
                           resamples = folds_4.1,
                           metrics = metric_set(mae, rmse))

collect_metrics(folds_4.2)

predictions_4.1 <- boost_4.1 %>%
  predict(new_data = data_3.1.test)%>%
  bind_cols(data_3.1.test)

mae(predictions_4.1,  estimate = .pred, truth = burden_CO2_national)
rmse(predictions_4.1, estimate = .pred, truth = burden_CO2_national)

# Optimize the boosted ensemble - tune()

boost_4.2 <- boost_tree(
  trees = 500, 
  learn_rate  = tune(),
  tree_depth  = tune(),
  sample_size = tune())%>%
  set_mode("regression")%>%
  set_engine("xgboost")

# Create a grid

grid_4 <- grid_regular(parameters(boost_4.2),
                       levels = 2) # how many levels for each parameter
grid_4.1 <- grid_random(parameters(boost_4.2),
                        size = 8)

# Create folds


# Pass it to tune_grid()

tune_4.2 <- tune_grid(boost_4.2,
                      burden_CO2_national ~ .,
                      resamples = vfold_cv(data_3.1.training, v = 10),
                      grid = grid_4,
                      metrics = metric_set(mae, rmse))

# Visualize tuning results
autoplot(tune_4.2)

# Best parameters: select_best() and finalize_model()

tune_4.2.1 <- select_best(tune_4.2) # Select best model

tune_4.2.2 <- finalize_model(boost_4.2, tune_4.2.1)

# Train the final model: fit()

tune_4.2.3 <- tune_4.2.2 %>%
  fit(burden_CO2_national ~ .,
      data = data_3.1.training)

predictions_4.1 <- predict(tune_4.2.3, new_data = data_3.1.test)%>%
  rename(boosted_pred = .pred)

vip(tune_4.2.3)

# What is the best model?

# Model comparison - compare models: decision trees, bagged trees, random forest, boosted trees

evaluation_4.1 <- data_3.1.test %>%
  select(burden_CO2_national) %>%
  bind_cols(predictions_2.1,
            predictions_3.3,
            predictions_4.1)

# How to evaluate roc_auc equivalent for regression trees?
# (evaluation_4.1, truth = burden_CO2_national, estimate = boosted_pred)

bind_rows(bagged_trees  = mae(evaluation_4.1, truth = burden_CO2_national, bagging_pred),
          random_forest = mae(evaluation_4.1, truth = burden_CO2_national, random_pred),
          boosted_trees = mae(evaluation_4.1, truth = burden_CO2_national, boosted_pred),
          bagged_trees  = rmse(evaluation_4.1, truth = burden_CO2_national, bagging_pred),
          random_forest = rmse(evaluation_4.1, truth = burden_CO2_national, random_pred),
          boosted_trees = rmse(evaluation_4.1, truth = burden_CO2_national, boosted_pred),
          .id = "model")

# Also with pivot_longer()
# roc_curve() and autoplot()

# possibly to be combined with workflows()

# 5. Machine learning in tidyverse ####

# Use lists and purrr

# nest() to create list columns

data_5 <- data_0 %>%
  group_by(Country)%>%
  nest()

# map() to work with list columns

data_5.1 <- data_5 %>%
  mutate(mean_CO2  = map(data, ~ mean(.x$burden_CO2_national)),
         model_1   = map(data, ~lm(burden_CO2_national ~ hh_expenditures_USD_2014 + urban_01, data = .x)),
         tidy_1    = map(model_1, ~tidy(.)),
         glance_1  = map(model_1, ~glance(.)),
         augment_1 = map(model_1, ~augment(.)))

# unnest() and map_*() to simplify list columns

data_5.2 <- data_5.1 %>%
  select(Country, augment_1)%>%
  unnest(augment_1)

# With multiple linear regression we may need adjusted R2 instead of R2

# Splitting --> Train-Test Split

data_5.3 <- initial_split(data_0, prob = 0.75)
data_5.3.training <- training(data_5.3)
data_5.3.testing  <- testing(data_5.3)

data_5.3.1.training <- vfold_cv(data_5.3.training, v = 10)

data_5.3.1 <- data_5.3.1.training %>%
  mutate(train    = map(splits, ~ training(.x)),
         validate = map(splits, ~ testing(.x)))

# Model with training dataset

data_5.3.1.1 <- data_5.3.1 %>%
  mutate(model           = map(train, ~ lm(burden_CO2_national ~ urban_01 + hh_expenditures_USD_2014, data = .x)),
         # Validate with validate dataset
         validate_actual    = map(validate, ~ .x$burden_CO2_national),
         validate_predicted = map2(.x = model, .y = validate, ~ predict(.x, .y)),
         # Compare and measure performance
         validate_mae       = map2_dbl(validate_actual, validate_predicted, ~ mae(actual = .x, predicted = .y)))

# With random forest

data_5.3.1.2 <- data_5.3.1 %>%
  crossing(mtry = 1:2)%>%
  mutate(model_rf = map2(train, mtry, ~ranger(burden_CO2_national ~ urban_01 + hh_expenditures_USD_2014,
                                       data = .x, mtry = .y, seed = 2023, num.trees = 100)),
         validate_predicted = map2(model_rf, validate, ~ predict(.x, .y)$predictions),
         validate_actual = map(validate, ~ .x$burden_CO2_national),
         validate_mae    = map2_dbl(validate_actual, validate_predicted, ~ mae(actual = .x, predicted = .y)))

data_5.3.1.3 <- data_5.3.1.2 %>%
  group_by(mtry)%>%
  summarise(mean_mae = mean(validate_mae))%>%
  ungroup()

# Select best performing model and evaluate with test set

model_5.3.1 <- ranger(formula = burden_CO2_national ~ urban_01 + hh_expenditures_USD_2014,
                      data = data_5.3.training,
                      mtry = 1, num.trees = 100, seed = 2023)
data_5.3.2.1 <- data_5.3.testing$burden_CO2_national
data_5.3.2.2 <- predict(model_5.3.1, data_5.3.testing)$predictions

mae(data_5.3.2.1, data_5.3.2.2)

# For classification models: accuracy(), precision() or recall()
# Compare multiple models and selected the best one

# 6. Modeling with tidymodels in R ####

# Data resampling - randomly splitting dataset in training and testing dataset (0.75)
# Training data is used for feature engineering, model fitting and tuning. 
# Test data is used for evaluating model performance.

data_6.1   <- initial_split(data_0.1, prob = 0.75, strata = burden_CO2_national)
data_6.1.1 <- training(data_6.1)
data_6.1.2 <- testing(data_6.1)

# Feature engineering with recipes()

data_6.2 <- recipe(burden_CO2_national ~ .,
                   data = data_6.1.1)%>%
  # Log-transformation
  step_log(hh_expenditures_USD_2014, base = 10)%>%
  prep(training = data_6.1.1)%>%
  bake(new_data = NULL)

# create correlation matrices - remove highly correlated variables

data_6.2.1 <- recipe(burden_CO2_national ~ .,
                     data = data_6.1.1)%>%
  step_corr(all_numeric(), threshold = 0.9)%>%
  prep(training = data_6.1.1)%>%
  bake(new_data = NULL)

# Imputation of missing data

# Data transformation - centering and scaling numeric variables

data_6.2.2 <- recipe(burden_CO2_national ~ .,
                     data = data_6.1.1)%>%
  step_normalize()%>%
  prep(training = data_6.1.1)%>%
  bake(new_data = NULL)

# One-Hot-Encoding (dummifying)

data_6.2.3 <- recipe(burden_CO2_national ~ .,
                     data = data_6.1.1)%>%
  step_dummy(District, Province)%>%
  prep(training = data_6.1.1)%>%
  bake(new_data = NULL)

# Model fitting - parsnip

model_6.1 <- linear_reg()%>%
  set_engine('lm')%>%
  set_mode('regression')%>%
  fit(burden_CO2_national ~ hh_expenditures_USD_2014 + urban_01 + factor(District),
      data = data_6.1.1)%>%
  predict(new_data = data_6.1.2)%>%
  bind_cols(data_6.1.2$burden_CO2_national)

# Evaluating model performance

rmse(model_6.1$...2, model_6.1$.pred)

model_6.2 <- linear_reg()%>%
  set_engine('lm')%>%
  set_mode('regression')%>%
  last_fit(burden_CO2_national ~ hh_expenditures_USD_2014 + urban_01 + factor(District),
      split = data_6.1)%>%
  collect_metrics()

# logistic_reg() %>% set_engine('glm') %>% set_mode('classification') for logistic regression

# Model tuning
# Model evaluation



# 7. Feature engineering - prepare and preprocess data ####

# Split into training and tests set - initial_split() + training() + testing()

# Declare our model 

# Set up recipe
# Imputation of NAs through k-nearest neighbours step_impute_knn()
# log-transformation, normalization, 
# Box-cox transformation / Yeo-Johnson transformation - non-normal variable closer to normal
# Including polynomials (such as squares), including percentiles
# Reducing dimensionality - zero variance columns should be deleted. 
# Near-zero variance features: Remove if there are very few unique values and percentage is < 1 --> step_nzv()
# --> PCA as a solution + step_normalize() + step_pca()
# Feature hashing - step_dummy_hash() - creating factors with "empty" values

# Removing irrelevant variables may be necessary - select only important variables from feature-dataframe

# Too complex models may be prone to overfitting --> lasso or ridge for regression / elastic net

# Bundle in workflow - workflow() + add_model() + add_recipe()

# Fit workflow - fit()

# Assess model performance - tidy() + glance() + augment()

# extract_fit_parsnip() + vip() package helps to plot important variables - less important variables may be removed from final model
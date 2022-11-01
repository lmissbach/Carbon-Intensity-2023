set.seed(2022)

library(tidymodels)
library(rsample)
library(ranger)
library(baguette)
library(randomForest)
# Splitting

# use initial_split() with proportion and strata-parameter
# Create training and testing sets with training() and testing()

# decision_tree(tree_depth, min_n,)
# set_engine("rpart)
# set_mode("regression")

# tune with tune()-function
# create tuning grid

# select_best()
# finalize_model()
# fit() - build the final model

# collect_metrics

# Calculating ROC / AUC
# predict(type = "prob)

# baguette::bag_tree() with times = parameters
# Works similar to regression tree techniques

# rand_forest( mtry = , trees , min_n)

# boosted models
# boost_tree()%>%set_engine("xgboost")
#predictions <- boost_tree() %>%
#  set_mode("classification") %>%
#  set_engine("xgboost") %>% 
#  fit(still_customer ~ ., data = customers_train) %>%
#  predict(new_data = customers_train, type = "prob") %>% 
#  bind_cols(customers_train)
#-------------------------------------------------------------------------
# Problem 3
#-------------------------------------------------------------------------

library(glmnet)
library(tidymodels)
library(tidyverse)

#-------------------------------------------------------------------------
# Problem 4
#-------------------------------------------------------------------------

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", 
                      col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

#-------------------------------------------------------------------------
# Problem 5
#-------------------------------------------------------------------------
set.seed(123456)

#-------------------------------------------------------------------------
# Problem 6
#-------------------------------------------------------------------------

housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test <- testing(housing_split)

#-------------------------------------------------------------------------
# Problem 7
#-------------------------------------------------------------------------

housing_recipe <- recipe(medv ~ ., data = housing) %>% # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox 
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
  ptratio:b:lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b, lstat,dis,nox, degree=6)

housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)

housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# the dimensions are 404x75
# there are 75 variables in the new data, which is 61 more than the original data

# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv) 
housing_test_x <- housing_test_prepped %>% selec t(-medv) 
housing_train_y <- housing_train_prepped %>% select( medv) 
housing_test_y <- housing_test_prepped %>% select( medv)

#-------------------------------------------------------------------------
# Problem 8 - LASSO Prediction
#-------------------------------------------------------------------------

# tuning the penalty parameter 

tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

lambda_grid <- grid_regular(penalty(), levels = 50)
# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) 

rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

final_lasso <- finalize_workflow(rec_wf, best_rmse)

last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print

top_rmse %>% print(n = 1)

# finding the RMSE and R^2 in and out of sample taking the penatly given by tuning

lasso_spec <- linear_reg(penalty=0.00139,mixture=1) %>%       # Specify a model
  set_engine("glmnet") %>%   # Specify an engine: lm, glmnet, stan, keras, spark
  set_mode("regression") # Declare a mode: regression or classification

lasso_fit <- lasso_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

# predict RMSE in sample
lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# RMSE in sample is 0.137

# predict RMSE out of sample
lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# RMSE out of sample is 0.188

# predict R2 in sample
lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# R2 in sample is 0.891

# predict R2 out of sample
lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# in-sample RMSE was 0.137
# out-of-sample RMSE is 0.188
# in-sample R^2 was 0.891
# out-of-sample R^2 is 0.768

#-------------------------------------------------------------------------
# Problem 8 - RIDGE Prediction
#-------------------------------------------------------------------------

# tuning using ridge

tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0      # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

lambda_grid <- grid_regular(penalty(), levels = 50)
# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) 

rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

final_ridge <- finalize_workflow(rec_wf, best_rmse)

last_fit(final_ridge, split = housing_split) %>%
  collect_metrics() %>% print

top_rmse %>% print(n = 1)

# running to get estimated in-sample and out-of-sample RMSE

ridge_spec <- linear_reg(penalty=0.0373,mixture=0) %>%       # Specify a model
  set_engine("glmnet") %>%   # Specify an engine: lm, glmnet, stan, keras, spark
  set_mode("regression") # Declare a mode: regression or classification

ridge_fit <- ridge_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

# predict RMSE in sample
ridge_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# RMSE in sample is 0.140

# predict RMSE out of sample
ridge_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  print

# RMSE out of sample is 0.180

# predict R2 in sample
ridge_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# R2 in sample is 0.884

# predict R2 out of sample
ridge_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rsq_trad(truth,`.pred`) %>%
  print

# in-sample RMSE was 0.140
# out-of-sample RMSE is 0.180
# in-sample R^2 was 0.884
# out-of-sample R^2 is 0.787

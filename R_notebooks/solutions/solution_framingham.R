
## import and minimalist EDA
## looking at the data a bit 
print("TenYearCHD value counts:")
print( table(framingham$TenYearCHD) )
print("we can see there is a lot of imbalance")
print("")
print("Fraction of NAs:")
print( framingham %>% is.na() %>% colSums() )
print("some NA, but not a huge amount")

## splitting train and test

split <- initial_split(framingham, prop = 0.75, strata = TenYearCHD)
heart_train <- training(split)
heart_test  <- testing(split)

print("train:")
print(table(heart_train$TenYearCHD ))
print('')
print("test:")
print(table(heart_test$TenYearCHD ))

## setting up the pre-processing recipe, cross-validation setup, and metric set (these will be shared between KNN and LR):

## recipe
framingham_recipe <- recipe( TenYearCHD ~ . , data = heart_train) %>%
  step_rose(TenYearCHD) %>%                  # handle imbalance
  step_impute_mean(all_predictors()) %>%   # imputation
  step_normalize(all_predictors())           # scaling


## Cross-Validation setup
folds <- vfold_cv(heart_train, strata = TenYearCHD,v = 5)


## setup a number of metrics: 
cls_metrics <- metric_set(accuracy,
                          f_meas,   # f_meas is f1 by default
                          roc_auc,
                          recall,
                          metric_tweak("recall_second", recall , event_level='second')
)


# KNN : 

KNN_model <- nearest_neighbor(neighbors = tune(),
                              weight_func = tune(),
                              mode = 'classification') 

KNN_wf <- 
  workflow() %>%
  add_model( KNN_model ) %>%
  add_recipe( framingham_recipe )

## prepare the grid of hyper-parameter values to test:
KNN_grid <- grid_regular( neighbors(range = c(5,200)) , 
                          weight_func( values = c('rectangular',
                                                  'triangular',
                                                  'optimal')),
                          levels = 10)


KNN_res <- 
  KNN_wf %>% 
  tune_grid(
    resamples = folds,
    grid = KNN_grid,
    metrics = cls_metrics
  )


# Logistic regression :

LR_model <- logistic_reg( mode = "classification",
                          engine = "glmnet",
                          penalty = tune(),
                          mixture = tune() )

LR_wf <- 
  workflow() %>%
  add_model( LR_model ) %>%
  add_recipe( framingham_recipe )


LR_grid <- grid_regular( penalty(range=c(-5,2), 
                                 trans = transform_log10()) , 
                         mixture(range = c(0, 1)),
                         levels = c(15,6))

LR_res <- 
  LR_wf %>% 
  tune_grid(
    resamples = folds,
    grid = LR_grid,
    metrics = cls_metrics
  )

# looking at the best models 

LR_res %>% show_best(metric='roc_auc')

KNN_res %>% show_best(metric='roc_auc')

# evaluating the best model on the test set

## Logistic regression seems to have performed better with respect to the ROC AUC metric

best_model <- LR_res %>% select_best(metric = "roc_auc")

final_LR <- LR_wf %>% 
  finalize_workflow(best_model) %>% # apply hyper-parameters
  last_fit(split) %>%               # fit with the training data
  extract_workflow(final_fit)       # extract the workflow

truth = heart_test$TenYearCHD
pred  = ( final_LR  %>% predict( heart_test ) )$.pred_class
pred_probas = ( final_LR  %>% predict( heart_test , type='prob' ) )$.pred_1

print(paste('accuracy:',accuracy_vec(truth , pred)))
print(paste('ROC AUC:',roc_auc_vec(truth , pred_probas)))
print(paste('recall class 0:',recall_vec(truth , pred , event_level = 'first')))
print(paste('recall class 1:',recall_vec(truth , pred , event_level = 'second')))
conf_mat( table( truth , pred ) )

# looking at the best models feature importance

final_LR %>% 
  extract_fit_parsnip() %>% 
  vip()

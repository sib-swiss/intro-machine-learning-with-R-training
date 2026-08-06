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
  step_upsample(TenYearCHD) %>%              # handle imbalance
  step_normalize(all_predictors())           # scaling


## Cross-Validation setup
folds <- vfold_cv(heart_train, strata = TenYearCHD,v = 5)


framingham_model <- rand_forest(
  mode = "classification",
  trees = 500,
  min_n = tune()
) %>% 
  set_engine( engine = 'ranger', importance = "impurity" ) ## important to get the feature importance later

framingham_wf <- 
  workflow() %>%
  add_model( framingham_model ) %>%
  add_recipe( framingham_recipe )

## prepare the grid of hyper-parameter values to test:
framingham_grid <- grid_random( min_n(range = c(10, 150)), 
                                size = 20)



framingham_RF_res <- 
  framingham_wf %>% 
  tune_grid(
    resamples = folds,
    grid = framingham_grid
  )


print( framingham_RF_res %>% 
  show_best(metric = "roc_auc", n = 5) )


## choose a combination of hyperparameter
best_model <- framingham_RF_res %>% select_best(metric = "roc_auc")

final_RF <- framingham_wf %>% 
  finalize_workflow(best_model) %>% # apply hyper-parameters
  last_fit(split)                   # fit with the train data

print(final_RF %>%
  collect_metrics() )


test_predictions = final_RF %>% collect_predictions()
print( table( test_predictions$TenYearCHD , test_predictions$.pred_class ) )

roc_curve(test_predictions , .pred_1 , truth = TenYearCHD  ) %>% autoplot()


final_RF %>% 
  extract_fit_parsnip() %>% 
  vip()




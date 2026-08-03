breast_recipe <- recipe( malignant ~ . , data = BC_train) %>%
  step_rose( malignant ) %>%  # add subsampling step
  step_normalize(all_predictors()) 


breast_model <- logistic_reg( mode = "classification",
                              engine = "glmnet",
                              penalty = tune(),
                              mixture = tune() )

breast_wf <- 
  workflow() %>%
  add_model( breast_model ) %>%
  add_recipe( breast_recipe )


breast_grid <- grid_regular( penalty(range=c(-5,2), 
                                     trans = transform_log10()) , 
                             mixture(range = c(0, 1)),
                             levels = c(15,6))

## setup a number of metrics: 
cls_metrics <- metric_set(accuracy,
                          f_meas,   # f_meas is f1 by default
                          roc_auc,
                          recall,
                          metric_tweak("recall_second", recall , event_level='second')
)


breast_LR_res <- 
  breast_wf %>% 
  tune_grid(
    resamples = folds,
    grid = breast_grid,
    metrics = cls_metrics ## add metrics here
  )



breast_LR_res %>% 
  collect_metrics(type='wide') %>% 
  arrange( desc( f_meas )  ) %>%
  select( !.config ) %>%
  head( n = 3 )
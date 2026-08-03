# we add a normalization step, so we need a recipe:
blobs_recipe <- recipe( label ~ . , data = X_train) %>%
  step_normalize(all_predictors())

blobs_model <- nearest_neighbor(neighbors = tune(),
                                weight_func = tune(),
                                mode = 'classification') 

blobs_wf <- 
  workflow() %>%
  add_model( blobs_model ) %>%
  add_recipe( blobs_recipe )

## prepare the grid of hyper-parameter values to test:
blobs_grid <- grid_regular( neighbors(range = c(1,30)) , 
                            weight_func( values = c('rectangular',
                                                    'triangular',
                                                    'optimal')),
                            levels = 30)

## Cross-Validation setup
folds <- vfold_cv(X_train, v = 5)


blobs_res <- 
  blobs_wf %>% 
  tune_grid(
    resamples = folds,
    grid = blobs_grid,
    metrics = metric_set( accuracy )
  )

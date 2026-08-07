
split <- initial_split(dfTT, prop = 0.7)
X_train <- training(split)
X_test  <- testing(split)

print(paste("train set size:",nrow(X_train)))
print(paste("test set size:" ,nrow(X_test)))


# we add a normalization step, so we need a recipe:
potato_recipe <- recipe( Flesh.Colour ~ . , data = X_train) %>%
  step_normalize(all_predictors()) %>%
  step_pca( all_predictors() , num_comp = 15 )

# Cross-Validation setup
folds <- vfold_cv(X_train, v = 10)

# LR
LR_model <- linear_reg(penalty = tune(),  
                       mixture = tune()) %>% 
  set_engine("glmnet") 

LR_wf <- 
  workflow() %>%
  add_model( LR_model ) %>%
  add_recipe( potato_recipe )

LR_grid <- grid_regular(penalty(range = c(-3,2) ),
                        mixture(),
                        levels = 10) ## 10 levels per hyper-parameter

LR_res <- 
  LR_wf %>% 
  tune_grid(
    resamples = folds,
    grid = LR_grid,
    metrics = metric_set( rsq, mae )
  )


# RF
RF_wf = workflow() %>% 
  add_model( rand_forest( mode = 'regression', 
                          min_n =  tune() 
  ) ) %>% 
  add_recipe( potato_recipe )

RF_res <- 
  RF_wf %>% 
  tune_grid(
    resamples = folds,
    grid = grid_random( min_n( range = c(2,100) ),
                        size = 20 ),
    metrics = metric_set( rsq, mae )
  )

# show best models
print( LR_res %>% show_best() )
print( RF_res %>% show_best() )


# LR returns better performances
# let's go get that
best_model <- LR_res %>% select_best(metric = "rsq")

final_wf <- 
  LR_wf %>% 
  finalize_workflow(best_model)

final_fit <- 
  final_wf %>%
  last_fit(split) 

final_fit %>%
  collect_metrics()

test_predictions = final_fit %>% collect_predictions()
ggplot(test_predictions , aes(x=Flesh.Colour , y = .pred)) + geom_point()


## interpretation
final_LR <- extract_workflow(final_fit)

## extract model coefficients, but... they are all about Principal Components
coef_df = tidy( final_LR ) %>% filter(term != "(Intercept)") %>% 
  mutate( coef_abs = abs(estimate) ) %>% 
  arrange( desc( coef_abs ) ) 

## so we need to extract the contribution of the original features to each PCs
potato_recipe = potato_recipe %>% prep()

## matrix where columns are PCs and rows are original features 
rot_mat <- tidy( potato_recipe , number = 2) %>%
  select(terms, component, value) %>%
  pivot_wider(names_from = component, values_from = value) %>%
  column_to_rownames("terms") %>%
  as.matrix()

## one annoyance, in the model coef they use PC01 while in the recipe output
## they use PC1
## so we need to transform it...
library(stringr)
coef_df$term = str_replace( coef_df$term , "([^0-9])0" , "\\1" )

original_coefficient = rot_mat[ , coef_df$term ] %*% coef_df$estimate 
original_coefficient


## performing the PCA
aml_recipe = recipe( X_aml , auc ~ . ) %>% # set AUC as response
  step_normalize( all_numeric_predictors() ) %>% 
  step_pca( all_predictors(),  num_comp = 17,
            options = list(retx=TRUE) ) %>%
  prep()

df_aml_pca = aml_recipe %>% bake( X_aml )

## cumulative variance % 
tidy( im_recipe , number = 3 , type = 'variance' ) %>%
  filter( terms ==  "cumulative percent variance") %>%
  ggplot( aes(x = component, y = value)) +
  geom_line() +
  ylab("cumulative percent variance")

# We see that with 5 components we already have >80% of the variance in the data

## biplot
prcomp_obj = aml_recipe$steps[[2]]$res

ggbiplot(prcomp_obj, 
         groups = X_aml$auc,
         choices = c(1,2),
         alpha = 0.8,
         varname.size = 3,
         varname.color = viridisLite::turbo(length( prcomp_obj$sdev )),
         var.factor = 1
)

## It looks like the first component separates the high-auc and low-auc samples.
## each gene seems to contribute a clear positive / negative signal to PC1
library(forcats)

tidy( aml_recipe , number = 2 , type = 'coef' ) %>%
  filter( component %in% c("PC1") ) %>%
  mutate(terms = fct_reorder(terms, desc(value))) %>%
  ggplot(aes( x = terms , y = value , fill = value<0 )) + 
  geom_bar(stat = "identity") + 
  coord_flip()


## extra task - UMAP
im_umap = umap( df_aml_pca %>% select(starts_with("PC")) , 
                random_state = 2026,
                n_neighbors = 15)

ggplot( NULL , aes( x = im_umap$layout[,1] , 
                    y = im_umap$layout[,2],
                    col = X_aml$auc) ) + 
  geom_point()

# we get a profile very close to the PCA, which is not unexpected when the data is "simple"
  

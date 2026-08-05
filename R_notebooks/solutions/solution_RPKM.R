#  1. perform a PCA and visualize the results, only keep 500 components

## performing the PCA
rpkm_recipe = recipe( X_rpkm , ~ . ) %>% 
  step_normalize( all_numeric_predictors() ) %>% 
  step_pca( all_predictors(),  num_comp = 500 ) %>%
  prep()

rpkm_pca = rpkm_recipe %>% bake( X_rpkm )

## cumulative variance % 
tidy( rpkm_recipe , number = 2 , type = 'variance' ) %>%
  filter( terms ==  "cumulative percent variance") %>%
  ggplot( aes(x = component, y = value)) +
  geom_line() +
  ylab("cumulative percent variance")


## plot of PC axes 1 and 2
ggplot(rpkm_pca , aes( x = PC001 , y = PC002 , col = index_colors) ) +
  geom_point()

## 3D plot 
library(plotly)
fig <- plot_ly(rpkm_pca, 
               x = ~PC001, 
               y = ~PC002, 
               z = ~PC003, 
               color = index_colors)
fig

##2. try to cluster the cells using K-Means or Hierarchical clustering. 
##   Try several parameter values and evaluate your clustering with a metric.

### hierarchical clustering
hc_sweep = clusterSweep( rpkm_pca ,
                         BLUSPARAM = HclustParam(
                           method = 'single',
                           dist.fun = dist,
                           cut.params = list( k = 3 )
                         ),
                         method = c( 'single' , 'average' , 'complete'  ),
                         cut.params = list( k = 2:20 )
)

## compute silhouette
sil <- vapply(as.list(hc_sweep$clusters), 
              function(clustering) mean(approxSilhouette(rpkm_pca,
                                                         clustering)$width), 0)

## compute ARIs
ARIs <- vapply(as.list(hc_sweep$clusters), 
               function(clustering) pairwiseRand(index_colors, 
                                                 clustering, mode= 'index'), 0)

scores_HC = data.frame( 
  K = unlist( hc_sweep$parameters$cut.params ),
  method = hc_sweep$parameters$method,
  silhouette = sil, 
  ARI = ARIs
)

### Kmeans
km_sweep = clusterSweep( rpkm_pca ,
                         BLUSPARAM = KmeansParam(
                           centers = 3
                         ),
                         centers = 2:20
)

## compute silhouette
sil <- vapply(as.list(km_sweep$clusters), 
              function(clustering) mean(approxSilhouette(rpkm_pca,
                                                         clustering)$width), 0)

## compute ARIs
ARIs <- vapply(as.list(km_sweep$clusters), 
               function(clustering) pairwiseRand(index_colors, 
                                                 clustering, mode= 'index'), 0)

scores_KM = data.frame( 
  K = km_sweep$parameters$centers,
  method = rep( 'Kmean' , length(sil) ),
  silhouette = sil, 
  ARI = ARIs
)

all_scores = rbind(scores_HC , scores_KM  )

## plotting all scores
p1 <- ggplot( all_scores , aes( x = K, y = silhouette, col = method ) ) + geom_line()
p2 <- ggplot( all_scores , aes( x = K, y = ARI, col = method ) ) + geom_line()
grid.arrange(p1, p2, ncol=2)


## if we go with Kmean centers - 11 (best ARI)
km = kmeans( rpkm_pca , centers = 11 )
table( index_colors , km$cluster )

fig <- plot_ly(rpkm_pca, 
               x = ~PC001, 
               y = ~PC002, 
               z = ~PC003, 
               color = as.factor(km$cluster) )
fig


## 3. try the same thing, but use only the 50 first PCA components.
##    Do you get better results?

rpkm_pca = rpkm_pca[,1:50]

### hierarchical clustering
hc_sweep = clusterSweep( rpkm_pca ,
                         BLUSPARAM = HclustParam(
                           method = 'single',
                           dist.fun = dist,
                           cut.params = list( k = 3 )
                         ),
                         method = c( 'single' , 'average' , 'complete'  ),
                         cut.params = list( k = 2:20 )
)

## compute silhouette
sil <- vapply(as.list(hc_sweep$clusters), 
              function(clustering) mean(approxSilhouette(rpkm_pca,
                                                         clustering)$width), 0)

## compute ARIs
ARIs <- vapply(as.list(hc_sweep$clusters), 
               function(clustering) pairwiseRand(index_colors, 
                                                 clustering, mode= 'index'), 0)

scores_HC = data.frame( 
  K = unlist( hc_sweep$parameters$cut.params ),
  method = hc_sweep$parameters$method,
  silhouette = sil, 
  ARI = ARIs
)

### Kmeans
km_sweep = clusterSweep( rpkm_pca ,
                         BLUSPARAM = KmeansParam(
                           centers = 3
                         ),
                         centers = 2:20
)

## compute silhouette
sil <- vapply(as.list(km_sweep$clusters), 
              function(clustering) mean(approxSilhouette(rpkm_pca,
                                                         clustering)$width), 0)

## compute ARIs
ARIs <- vapply(as.list(km_sweep$clusters), 
               function(clustering) pairwiseRand(index_colors, 
                                                 clustering, mode= 'index'), 0)

scores_KM = data.frame( 
  K = km_sweep$parameters$centers,
  method = rep( 'Kmean' , length(sil) ),
  silhouette = sil, 
  ARI = ARIs
)

all_scores = rbind(scores_HC , scores_KM  )

## plotting all scores
p1 <- ggplot( all_scores , aes( x = K, y = silhouette, col = method ) ) + geom_line()
p2 <- ggplot( all_scores , aes( x = K, y = ARI, col = method ) ) + geom_line()
grid.arrange(p1, p2, ncol=2)

# we can see that silhouette scores are getting higher
# the ARI too is good
km = kmeans( rpkm_pca , centers = 9 )
table( index_colors , km$cluster )

fig <- plot_ly(rpkm_pca, 
               x = ~PC001, 
               y = ~PC002, 
               z = ~PC003, 
               color = as.factor(km$cluster) )
fig



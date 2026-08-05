## bluster clusterSweep helps us go through a number of clustering parameters
km_sweep = clusterSweep( df_aml_pca %>% select(starts_with("PC")) ,
                         BLUSPARAM = KmeansParam(centers = 3,
                                                 nstart = 3),
                         centers = 2:9 )

sil <- vapply(as.list(km_sweep$clusters), 
              function(clustering) mean(approxSilhouette(df_aml_pca %>% 
                                                           select(starts_with("PC")),
                                                         clustering)$width), 0)

ggplot(NULL , aes( x = 2:9 , y = sil ) ) +
  geom_line()

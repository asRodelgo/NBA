# --------------------------------------------
# Pre-compute tsne_points for all ages to save time as these computations don't really
# depend on the player selected. 
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- .tSNE_compute(num_iter, max_num_neighbors, a)
  write.csv(tsneBlock[[a]],paste0("data/tsneBlock","_",a,".csv"),row.names = FALSE)
}
# --------------------------------------------

.tSNE_computeRookies <- function(num_iter, max_num_neighbors){
  
  data_tsne <- .tSNE_prepareRookies()
  # calculate tsne-points Dimensionality reduction to 2-D
  if (nrow(data_tsne)>0){
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne[,-c(1:3)], 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=num_iter)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_pointsRookies.csv",row.names = FALSE)
  
}

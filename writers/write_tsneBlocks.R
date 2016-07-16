# --------------------------------------------
# Pre-compute tsne_points for all ages to save time as these computations don't really
# depend on the player selected. 
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- .tSNE_compute(num_iter, max_num_neighbors, a)
  write.csv(tsneBlock[[a]],paste0("data/tsneBlock","_",a,".csv"),row.names = FALSE)
}
# --------------------------------------------

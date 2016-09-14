# pre-calculate tsne points for all players and seasons
write_tSNE_compute_All <- function(num_iter, max_num_neighbors){
  
  # data_tsne contains the input data for tSNE filtered and cleaned up
  # calculate tsne-points Dimensionality reduction to 2-D
  
  library(doMC) # use parallel processing on this machine through "foreach"
  registerDoMC(2) # As far as I know my MAC works on 2 cores
  #data_tsne_sample <- dplyr::sample_n(data_tsne,1000)
  data_tsne_sample <- filter(data_tsne,Season > "1990-1991")
                             #%in% c("2012-2013","2013-2014","2014-2015","2015-2016"))
    #"2012-2013","2013-2014","2014-2015",
  
  if (nrow(data_tsne)>0){
    num_iter <- 400
    max_num_neighbors <- 10
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample[,-c(1:5)], 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points_All.csv",row.names = FALSE)
  
}

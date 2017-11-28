# t-SNE teams

.compute_teams_tSNE_points <- function(data, num_iter, max_num_neighbors,removeEffMin = TRUE){
  
  require(tsne)
  teamStats <- .computeTeamStats(data=data,removeEffMin = removeEffMin)
  data_tsne_sample <- teamStats %>% 
    select_if(is.numeric) %>%
    mutate_all(function(x) (x-min(x))/(max(x)-min(x)))
  
  if (nrow(data_tsne_sample)>0){
    #num_iter <- 500
    #max_num_neighbors <- 2
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample, 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  return(tsne_points)
}

# put together tsne_ready predicted to load at start of dashboards
.compute_tSNE_ready_teams <- function(data,removeEffMin = TRUE){
  
  tsne_points <- .compute_teams_tSNE_points(data,num_iter = 500,max_num_neighbors = 2,removeEffMin)
  
  # load data
  data_tsne_sample <- .computeTeamStats(data,removeEffMin) %>%
    select_if(is.character)
  # tsne_points are pre-calculated from write_tSNE_All.R and saved in data/ directory
  # using this function: tsne_points <- write_tSNE_compute_All()
  if (!nrow(data_tsne_sample)==nrow(tsne_points)){ # in case labels and coordinates have different sizes
    tsne_ready <- tsne_points
  } else {
    tsne_ready <- cbind(data_tsne_sample,tsne_points)
  }
  
  names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
  names(tsne_ready)[ncol(tsne_ready)] <- "y"
  
  return(tsne_ready)
}


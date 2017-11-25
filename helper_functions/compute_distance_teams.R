# calculate distance between 2 sample distributions
# Distance between 2 teams using Bhattacharyya distance: https://en.wikipedia.org/wiki/Bhattacharyya_distance

# data_tsne <- tsne_ready
# data_Players <- playerDashboard
# teamA <- "CLE"
# teamB <- "GSW"
# num_clusters <- 5
# useEffMin <- TRUE

BC_distance <- function(data_tsne = values$playersTSNE,data_Players = values$playersDatabase,
                        teamA,teamB,num_clusters = input$num_clusters,useEffMin = TRUE) {
  
  set.seed(123)
  playerCluster <- kmeans(data_tsne[, c("x","y")], num_clusters, nstart = 10, iter.max = 20)
  tsne_ready2 <- cbind(data_tsne, cluster = playerCluster$cluster) %>%
    merge(data_Players[,c("Player","Tm","effMin","Offense","Defense")], by=c("Player","Tm")) %>%
    mutate(Defense = -Defense) %>%
    group_by(Player) %>%
    mutate(effMin = ifelse(useEffMin, effMin, 1)) %>%
    ungroup()
  
  if (teamA == teamB) {
    partB <- filter(tsne_ready2, Tm == teamA) %>%
      mutate(Tm = paste0(teamA,"_2")) 
  
    BC <- filter(tsne_ready2, Tm == teamA) %>%
      bind_rows(partB) %>%
      group_by(cluster, Tm) %>%
      mutate(bc_Tm = sum(effMin*1000, na.rm=TRUE)) %>% # variation from the original: multiply by 10 and add 1 as my "sample sizes" are really small numbers representing weights
      ungroup() %>%
      distinct(cluster,Tm,bc_Tm) %>%
      group_by(cluster) %>%
      mutate(bc_cluster = sqrt(prod(bc_Tm))) %>%
      distinct(cluster,bc_cluster) %>%
      ungroup() %>%
      summarise(dist = 1/log1p(sum(bc_cluster,na.rm=TRUE))) # variation from the original, it's more intuitive for me to use BC as denominator
    
  } else {
    BC <- filter(tsne_ready2, Tm %in% c(teamA,teamB)) %>%
      group_by(cluster, Tm) %>%
      mutate(bc_Tm = sum(effMin*1000, na.rm=TRUE)) %>%
      ungroup() %>%
      distinct(cluster,Tm,bc_Tm) %>%
      group_by(cluster) %>%
      mutate(bc_cluster = sqrt(prod(bc_Tm))) %>%
      distinct(cluster,bc_cluster) %>%
      ungroup() %>%
      summarise(dist = 1/log1p(sum(bc_cluster,na.rm=TRUE))) # variation from the original, it's more intuitive for me to use BC as denominator
  }
  
  
  return(as.numeric(BC))
}

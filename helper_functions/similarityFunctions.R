# Find similar players ------------------------------
#
# Using t-sne algorithm, find players that have similar characteristics to a given player.
# The objective is to predict his performance in a given year based on the historical performance
# of similar players (see: Nate Silver's CARMELO or PECOTA systems)
#
# Ex: If I want to predict Pau Gasol numbers for the season he will turn 36, I will start
# with his numbers in the previous seasons and I will adjust according to the average
# evolution of similar players when they turned 36.
#
# Ex: To be able to assign predicted characteristics to a rookie player, I will use 
# average numbers for players that entered the league at the same age. This is a very bad
# approximation but the ultimate goal is to predict the number of wins of a team in a 
# given season and the effect of 1 or 2 rookie players should not be that important.
#
.tSNE_compute <- function(playerName, num_iter, max_num_neighbors, playerAge){
  
  # Players that changed teams in the season have a column Tm == "TOT" with their total stats
  # and because I don't care about the team, this should be enough filter
  # playerAge <- 30
  # num_iter <- 300
  # max_num_neighbors <- 10
  # playerName <- "Pau Gasol"
  data_tsne <- playersHist %>%
    group_by(Player,Season) %>%
    mutate(keep = ifelse(n() > 1, 1, 0), effectiveMin = MP/(48*82)) %>%
    filter(keep == 0 | Tm == "TOT") %>%
    filter(effectiveMin >= .15) %>% # Played at least 15% of total available minutes
    select(-Tm,-keep,-G,-GS,-MP)
  
  # Filter by selected age 
  data_tsne <- data_tsne %>%
    filter(Age == playerAge) %>%
    select(-Age) # redundant column, same value (playerAge) for all observations
  
  # some players can be the same age during 2 seasons. Pick the one with the most minutes played
  data_tsne <- data_tsne %>%
    group_by(Player) %>%
    filter(effectiveMin >= max(effectiveMin)-.0001)
  
  # t-sne doesn't like NAs. Impute by assigning 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  data_tsne <- data_tsne[,c(1,2,ncol(data_tsne)-1,ncol(data_tsne),3:(ncol(data_tsne)-2))]# reorder columns first
  for (i in 4:(ncol(data_tsne)-1)){
    data_tsne[is.na(data_tsne[,i]),i] <- 0
  }
  
  data_tsne <- as.data.frame(data_tsne)
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
  return(tsne_points) 
  
}

# compute colors for regions
.getColors <- function(playerAge,colVar){
  
  if (colVar == "Season"){
    colors <- rainbow(length(unique(data_tsne$Season)))
    names(colors) <- unique(data_tsne$Season)
  } else {
    colors <- rainbow(length(unique(data_tsne$Pos)))
    names(colors) <- unique(data_tsne$Pos)
  }
  return(colors)
}

# tsne chart ---------------------------------------------------------
.tSNE_plot <- function(playerName, num_iter, max_num_neighbors, playerAge, colVar){
  
  tsne_points <- .tSNE_compute(playerName, num_iter, max_num_neighbors, playerAge)
  if (length(tsne_points)>0){
    par(mar=c(0,0,0,0))
    plot(tsne_points,t='n', axes=FALSE, frame.plot = FALSE, xlab = "",ylab = ""); 
    graphics::text(tsne_points,labels=as.character(data_tsne$Player), col=.getColors(playerAge,colVar))
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

# tsne dist ---------------------------------------------------------
.tSNE_dist <- function(playerName, num_iter, max_num_neighbors, playerAge){
  
  tsne_points <- .tSNE_compute(playerName, num_iter, max_num_neighbors, playerAge)
  if (length(tsne_points)>0){
    # calculate the euclidean distance between the selected player and the rest
    dist_mat <- cbind(tsne_points,as.character(data_tsne$Player))
    dist_mat <- as.data.frame(dist_mat, stringsAsFactors=FALSE)
    dist_mat$V1 <- as.numeric(dist_mat$V1)
    dist_mat$V2 <- as.numeric(dist_mat$V2)
    distCou1 <- dist_mat[dist_mat$V3==playerName,1]
    distCou2 <- dist_mat[dist_mat$V3==playerName,2]
    dist_mat <- mutate(dist_mat, dist = sqrt((V1-distCou1)^2+(V2-distCou2)^2))
    # order by closest distance to selected player
    dist_mat <- arrange(dist_mat, dist)[,c(3,4)]
    names(dist_mat) <- c("Player","Euclid. distance")
  } else {
    dist_mat <- data_frame()
  }
  
  return(dist_mat)
} 

similarPlayers <- .tSNE_dist("Michael Jordan*",300,20,31)
head(similarPlayers,10)

simPlayers <- data.frame()
thisAge <- filter(playersHist, Season == "2015-2016", Player == playerName)$Age
for (t in (thisAge-5):thisAge){
  thisSimilar <- head(.tSNE_dist(playerName,300,20,t),20)
  thisSimilar$Age <- t
  if (nrow(simPlayers)>0){
    simPlayers <- bind_rows(simPlayers,thisSimilar)
  } else {
    simPlayers <- thisSimilar
  }
  print(thisSimilar)
}


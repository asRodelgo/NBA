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
.tSNE_prepare <- function(playerAge){
  # Players that changed teams in the season have a column Tm == "TOT" with their total stats
  # and because I don't care about the team, this should be enough filter
  # playerAge <- 34
  # num_iter <- 300
  # max_num_neighbors <- 20
  # playerName <- "Pau Gasol"
  data_tsne <- playersHist %>%
    group_by(Player,Season) %>%
    mutate(keep = ifelse(n() > 1, 1, 0), effMin = MP/3936, effFG = FG/(3936*effMin),
           effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
           eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
           effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
           effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
           effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
           effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
           effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
           effPTS = PTS/(3936*effMin)) %>%
    filter(keep == 0 | Tm == "TOT") %>%
    filter(effMin >= .15) %>% # Played at least 15% of total available minutes
    select(Player,Pos,Season,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
           FTPer = FT., starts_with("eff"),
           -Tm,-keep,-G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
           -BLK,-TOV,-PF,-FT,-STL,-PTS)
  
  # Filter by selected age 
  data_tsne <- data_tsne %>%
    filter(Age == playerAge) %>%
    select(-Age) # redundant column, same value (playerAge) for all observations
  
  # some players can be the same age during 2 seasons. Pick the one with the most minutes played
  data_tsne <- data_tsne %>%
    group_by(Player) %>%
    filter(effMin >= max(effMin)-.0001)
  
  # t-sne doesn't like NAs. Impute by assigning 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  for (i in 4:(ncol(data_tsne)-1)){
    data_tsne[is.na(data_tsne[,i]),i] <- 0
  }
  
  data_tsne <- as.data.frame(data_tsne)
  return(data_tsne)
  
}

.tSNE_compute <- function(num_iter, max_num_neighbors, playerAge){
  
  data_tsne <- .tSNE_prepare(playerAge)
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
.getColors <- function(num_iter, max_num_neighbors,playerAge,colVar){
  
  data_tsne <- .tSNE_prepare(playerAge)
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
  
  #tsne_points <- .tSNE_compute(num_iter, max_num_neighbors, playerAge)
  tsne_points <- tsneBlock[[playerAge]]
  if (length(tsne_points)>0){
    par(mar=c(0,0,0,0))
    plot(tsne_points,t='n', axes=FALSE, frame.plot = FALSE, xlab = "",ylab = ""); 
    graphics::text(tsne_points,labels=as.character(data_tsne$Player), col=.getColors(num_iter, max_num_neighbors,playerAge,colVar))
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

# tsne dist ---------------------------------------------------------
.tSNE_dist <- function(playerName, num_iter, max_num_neighbors, playerAge){
  
  data_tsne <- .tSNE_prepare(playerAge)
  #tsne_points <- .tSNE_compute(num_iter, max_num_neighbors, playerAge)
  tsne_points <- tsneBlock[[playerAge]]
  if (length(tsne_points)>0 & nrow(filter(data_tsne, Player == playerName))>0){
    # calculate the euclidean distance between the selected player and the rest
    dist_mat <- cbind(tsne_points,as.character(data_tsne$Player))
    dist_mat <- as.data.frame(dist_mat, stringsAsFactors=FALSE)
    dist_mat$V1 <- as.numeric(dist_mat$V1)
    dist_mat$V2 <- as.numeric(dist_mat$V2)
    distCou1 <- dist_mat[dist_mat[,3]==playerName,1]
    distCou2 <- dist_mat[dist_mat[,3]==playerName,2]
    dist_mat <- mutate(dist_mat, dist = sqrt((V1-distCou1)^2+(V2-distCou2)^2))
    # order by closest distance to selected player
    dist_mat <- arrange(dist_mat, dist)[,c(3,4)]
    names(dist_mat) <- c("Player","Euclid. distance")
  } else {
    dist_mat <- data_frame()
  }
  
  return(dist_mat)
} 

#similarPlayers <- .tSNE_dist("Russell Westbrook",300,20,27)
#head(similarPlayers,20)

# return similar players based on last 5 years performances
# For retired players this will return similar players according to their last 5 seasons
# as NBA player. Unless pickAge is explicitly entered
.similarPlayers <- function(playerName,numberPlayersToCompare, pickAge = 0){
  
  thisAgeFrame <- filter(playersHist, Player == playerName)
  
  if (nrow(thisAgeFrame) > 0){
    #thisAge <- filter(thisAgeFrame, Season == max(as.character(Season)))$Age
    minAge <- min(filter(thisAgeFrame, Player == playerName)$Age)
    maxAge <- max(filter(thisAgeFrame, Player == playerName)$Age)
    if (pickAge >= minAge & pickAge <= maxAge){
      thisAge <- pickAge
    } else{
      thisAge <- maxAge
    }
    
    simPlayers <- data.frame()
    t <- thisAge-5
    while (t <= thisAge){
      if (t >= minAge){
        thisSimilar <- .tSNE_dist(playerName,300,20,t)
        if (nrow(thisSimilar)>0){
          thisSimilar <- head(thisSimilar,numberPlayersToCompare)
          thisSimilar$Age <- t
          if (nrow(simPlayers)>0){
            simPlayers <- bind_rows(simPlayers,thisSimilar)
          } else {
            simPlayers <- thisSimilar
          }
          t <- t + 1
        } else {
          t <- t + 1
        }
      } else {
        t <- t + 1
      }
    }
    if (nrow(simPlayers)>0){ 
      simPlayers_5years <- simPlayers %>%
        filter(!(Player == playerName)) %>%
        group_by(Player) %>%
        mutate(numYears = n(),rank5years = mean(`Euclid. distance`)) %>%
        distinct(.keep_all=TRUE) %>%
        arrange(desc(numYears),rank5years)
      
      return(simPlayers_5years)
    } else { # Player didn't play enough minutes during the period considered
      return()
    }
    
  } else { # Player doesn't exist
    return()
  }
}

.predictPlayer <- function(playerName, numberPlayersToCompare,numberTeamsForVariation){

  # Top 10 more similar to selected player for past 5 years
  top10_similar <- head(.similarPlayers(playerName,numberPlayersToCompare),numberTeamsForVariation)$Player
  thisAgeFrame <- filter(playersHist, Player == playerName)
  thisAge <- max(filter(thisAgeFrame, Player == playerName)$Age)
  
  # Now calculate average variation in their stats when they went from current age to age + 1
  thisAgeData <- .tSNE_prepare(thisAge)
  namesKeep <- names(thisAgeData)
  names(thisAgeData)[2:ncol(thisAgeData)] <- sapply(names(thisAgeData)[2:ncol(thisAgeData)],
                                                    function(x) paste0(x,"_",thisAge))
  #thisAgeData$Age <- thisAge
  nextAgeData <- .tSNE_prepare(thisAge+1)
  #nextAgeData$Age <- thisAge + 1
  names(nextAgeData)[2:ncol(nextAgeData)] <- sapply(names(nextAgeData)[2:ncol(nextAgeData)],
                                                    function(x) paste0(x,"_",thisAge+1))
  
  ageData <- merge(thisAgeData,nextAgeData, by="Player")
  
  top10 <- ageData %>%
    filter(Player %in% top10_similar)
  
  top10_var <- data.frame()
  numCols <- ncol(thisAgeData)
  for (i in 1:nrow(top10)){
    top10_var[i,1] <- top10$Player[i]  
    for (j in 4:numCols){
      top10_var[i,j-2] <- ifelse(top10[i,j]==0,0,(top10[i,j+numCols-1]-top10[i,j])/top10[i,j])
    }
  }
  names(top10_var) <- namesKeep[c(1,4:length(namesKeep))]
  # Median variations for top 10 most similar players 
  top10_var <- summarise_each(top10_var, funs(median(.)),-Player)
  # Apply this variation to predict stats for this player for next season  
  predAgeData <- filter(thisAgeData, Player == playerName)
  for (i in 1:ncol(top10_var)){
    predAgeData[i+3] <- predAgeData[i+3]*(1+top10_var[i])
  }
  names(predAgeData) <- names(data_tsne)
  # Update the Season and Age of the player
  predAgeData$Season <- paste0(as.numeric(substr(predAgeData$Season,1,4))+1,"-",
                               as.numeric(substr(predAgeData$Season,1,4))+2)
  predAgeData$Age <- thisAge + 1
  
  return(predAgeData)

}

thisPrediction <- .predictPlayer("Klay Thompson",20,10)
head(.similarPlayers("Klay Thompson",20),10)

filter(playersHist, grepl("Ola",Player,fixed=TRUE))

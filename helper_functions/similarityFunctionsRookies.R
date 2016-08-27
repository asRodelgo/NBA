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
# Ex: To be able to assign predicted characteristics to a rookie player, I will do a
# similar approach. See functions related to rookies and draft
#
.tSNE_prepareRookies <- function(){
  # num_iter <- 300
  # max_num_neighbors <- 20
  # playerName <- "Stephen Curry"
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  rookieStats <- rookieStats[,1:29]
  rookieStats <- filter(rookieStats, !(College %in% c("International", "Europe")))
  
  rookieStatsHist <- read.csv("data/rookieStatsHist.csv", stringsAsFactors = FALSE)
  # transform rookieStatsHist stats to relative numbers
  rookieStatsHist <- rookieStatsHist %>%
    group_by(Player,Season) %>%
    mutate(MP = MP/G, FG = FG/G,
           FGA = FGA/G,X3P = X3P/G,X3PA = X3PA/G,
           X2P = X2P/G,X2PA = X2PA/G,
           FT = FT/G,FTA = FTA/G,
           ORB = ORB/G,DRB = DRB/G,
           TRB = TRB/G,AST = AST/G,
           STL = STL/G,BLK = BLK/G,
           TOV = TOV/G,PF = PF/G,
           PTS = PTS/G)
  # all together, ready for tsne
  collegeHist <- bind_rows(rookieStats,rookieStatsHist)
  
  data_tsne <- collegeHist %>%
    group_by(Player,Season) %>%
    mutate(effFG = FG,
           effFGA = FGA,eff3PM = X3P,eff3PA = X3PA,
           eff2PM = X2P,eff2PA = X2PA,
           effFTM = FT,effFTA = FTA,
           effORB = ORB,effDRB = DRB,
           effTRB = TRB,effAST = AST,
           effSTL = STL,effBLK = BLK,
           effTOV = TOV,effPF = PF,
           effPTS = PTS) %>%
    dplyr::select(Player,Pos,Season,Pick,starts_with("eff"))
  
  # t-sne doesn't like NAs. Impute by assigning the average of the variable. 
  # If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  data_tsne <- as.data.frame(data_tsne)
  for (i in 4:ncol(data_tsne)){
    data_tsne[is.na(data_tsne[,i]),i] <- mean(data_tsne[,i],na.rm=TRUE)
  }
  
  return(data_tsne)
  
}

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
  
  return(tsne_points) 
  
}

# compute colors for regions
.getColorsRookies <- function(num_iter, max_num_neighbors,colVar){
  #colVar <- "Pos"
  data_tsne <- .tSNE_prepareRookies()
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
.tSNE_plotRookies <- function(playerName, num_iter, max_num_neighbors, colVar){
  
  #tsne_points <- .tSNE_compute(num_iter, max_num_neighbors, playerAge)
  tsne_points <- read.csv("data/tsne_pointsRookies.csv",stringsAsFactors = FALSE)
  if (length(tsne_points)>0){
    par(mar=c(0,0,0,0))
    plot(tsne_points,t='n', axes=FALSE, frame.plot = FALSE, xlab = "",ylab = ""); 
    graphics::text(tsne_points,labels=as.character(data_tsne$Player), col=.getColorsRookies(num_iter, max_num_neighbors,colVar))
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

# tsne dist ---------------------------------------------------------
.tSNE_distRookies <- function(playerName){
  
  data_tsne <- .tSNE_prepareRookies()
  lastDraft <- max(data_tsne$Season)
  
  #tsne_points <- .tSNE_compute(num_iter, max_num_neighbors, playerAge)
  tsne_points <- read.csv("data/tsne_pointsRookies.csv",stringsAsFactors = FALSE)
  if (length(tsne_points)>0 & nrow(filter(data_tsne, Player == playerName))>0){
    # calculate the euclidean distance between the selected player and the rest
    dist_mat <- cbind(tsne_points,as.character(data_tsne$Player),data_tsne$Season)
    if (filter(data_tsne, Player == playerName)$Season == lastDraft){
      dist_mat <- dist_mat[!(data_tsne$Season==lastDraft & !(data_tsne$Player==playerName)),]
    }
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

# return predicted stats rookie season for any drafted player from college
.predictPlayerCollegeRookie <- function(playerName){
  
  data_tsne <- .tSNE_prepareRookies()
  similarPlayers <- .tSNE_distRookies(playerName)
  
  theirStats <- filter(data_tsne, Player %in% head(similarPlayers[-1,1],5))
  
  rookieNBAStats <- playersHist %>%
    group_by(Player) %>%
    filter(Season == min(as.character(Season)))
  rookieNBAStats <- as.data.frame(rookieNBAStats)
  thisSelection <- filter(rookieNBAStats, Player %in% theirStats$Player)
  thisSelectionPrep <- .tSNE_prepareSelected(thisSelection)
  this_numRows <- nrow(thisSelectionPrep)
  for (i in 4:ncol(thisSelectionPrep)){
    thisSelectionPrep[this_numRows+1,i] <- mean(thisSelectionPrep[1:this_numRows,i])
  }
  thisPlayer <- filter(data_tsne, Player == playerName)
  thisSelectionPrep$Player <- as.character(thisSelectionPrep$Player)
  thisSelectionPrep$Pos <- as.character(thisSelectionPrep$Pos)
  thisSelectionPrep$Season <- as.character(thisSelectionPrep$Season)
  thisSelectionPrep$Player[nrow(thisSelectionPrep)] <- thisPlayer$Player
  thisSelectionPrep$Pos[nrow(thisSelectionPrep)] <- thisPlayer$Pos
  thisSelectionPrep$Season[nrow(thisSelectionPrep)] <- as.character(playersNew$Season[1])
  
  playerPredicted <- filter(thisSelectionPrep, Player == playerName)
  
  return(playerPredicted)
}

#.predictPlayerCollegeRookie("Damian Jones")

# Non college players predicted stats
.predictPlayerNonCollegeRookie <- function(playerName){
  # Remove from playersHist those who played college and average out their stats by position.
  collegePlayersHist <- read.csv("data/collegePlayersHist.csv",stringsAsFactors = FALSE)
  collegePlayersHist <- collegePlayersHist %>%
    group_by(Player) %>%
    filter(Season == max(Season))
  onlyCollegeRookies <- dplyr::select(collegePlayersHist,Player)
  onlyCollegeRookies <- onlyCollegeRookies$Player
  
  nonCollegeRookies <- playersHist %>%
    filter(!(Player %in% onlyCollegeRookies)) %>%
    group_by(Player) %>%
    filter(Season == min(Season)) %>%
    distinct(Player, .keep_all=TRUE)
    
  # Calculate average stats for nonCollegeRookies on their first NBA season by position.
  # This will provide players without much statistical background in NBA or College with 
  # some prior stats. 
  # Can't do the above so I will do overall priors with no filter
  
  nonCollegeRookies_Stats <- nonCollegeRookies %>%
    filter(Season >= "1994-1995") %>%
    group_by() %>%
    summarise_at(c(5:(ncol(nonCollegeRookies)-1)),funs(mean(.,na.rm=TRUE)))

  # assign stats to input player and then adjust those stats like in .tsnePrepare
  # get player's postition
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  rookieStats <- rookieStats[,1:29]
  rookieStats <- filter(rookieStats, College %in% c("International", "Europe"))
  
  playerPredicted <- rookieStats %>%
    filter(Player == playerName) %>%
    dplyr::select(Player,Pos,Tm=Team,Pick) %>%
    mutate(Season = paste0(lastDraft,"-",lastDraft+1))
  playerPredicted <- bind_cols(playerPredicted,nonCollegeRookies_Stats)
  
  playerPredicted <- playerPredicted %>%
    mutate(MP = MP/G, FG = FG/G,
           FGA = FGA/G,X3P = X3P/G,X3PA = X3PA/G,
           X2P = X2P/G,X2PA = X2PA/G,
           FT = FT/G,FTA = FTA/G,
           ORB = ORB/G,DRB = DRB/G,
           TRB = TRB/G,AST = AST/G,
           STL = STL/G,BLK = BLK/G,
           TOV = TOV/G,PF = PF/G,
           PTS = PTS/G)
  
  playerPredicted <- playerPredicted %>%
    mutate(effFG = FG,
           effFGA = FGA,eff3PM = X3P,eff3PA = X3PA,
           eff2PM = X2P,eff2PA = X2PA,
           effFTM = FT,effFTA = FTA,
           effORB = ORB,effDRB = DRB,
           effTRB = TRB,effAST = AST,
           effSTL = STL,effBLK = BLK,
           effTOV = TOV,effPF = PF,
           effPTS = PTS) %>%
    dplyr::select(Player,Pos,Season,Pick,starts_with("eff"))
  
  return(playerPredicted)
  
}



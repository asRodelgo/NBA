# Phase 2 ------------------------------------
# Once rosters are updated (Phase 1), predict avg points and avg points against
# per team for the new season or for a season in the past (back to 1979-1980)
# compute avg PTS as offensive power and PTSA as defensive power
.computePower <- function(data = playersNew, Off_or_Def, thisTeam = "All", defaultMinutes = NULL, removeEffMin = TRUE, 
                          actualOrPredicted = "actual", maxs_vector = NULL, mins_vector = NULL){
  
  # specifically, this function will prepare playersNew dataset by default
  # It is understood, playersNew is the updated rosters at the beginning of a new season
  if (actualOrPredicted=="actual"){ # whether actual data (before prediction) or predicted data
    playersSumm <- .prepareModelPrediction(data = data, thisTeam)  
  } else {
    playersSumm <- .prepareModelOncePredicted(data_team = data, thisTeam = thisTeam)  
  }
  
  # effMin is 1 of the variables that get averaged weighted by effMin, in case it adds noise to the
  # neural network 
  if (!is.null(defaultMinutes)) { 
    playersSumm$effMin <- defaultMinutes
  }
  if (removeEffMin & ncol(select(playersSumm, one_of("effMin")))>0) { 
    playersSumm <- select(playersSumm, -effMin)
  }
  ## Strip linearly relationed columns: FG, FGA, FG%,3P%,2P%,FT%,effFG%, effPTS
  #playersSumm <- select(playersSumm, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
  ## End of Strip
  # scale the variables the same way the training dataset was scaled so the nnet makes sense
  # scaleMaxMin needs a reference column (team, player, etc.). I need to make sure it's the first column
  playersSumm <- select(playersSumm, team_season, everything())
  # The scale has to be preserved all the way during trades, otherwise, every single change 
  # in the composition of rosters will trigger a new scale and Offense and Defense powers
  # will be messed up
  if ((!is.null(maxs_vector)) & (!is.null(mins_vector))) {
    if (Off_or_Def == "PTS"){
      maxs <- maxs_vector[-nrow(maxs_vector),1]
      mins <- mins_vector[-nrow(maxs_vector),1]
    } else {
      maxs <- maxs_vector[-nrow(maxs_vector),2]
      mins <- mins_vector[-nrow(maxs_vector),2]
    }
  } else {
    scaleMaxMin <- .getScaleLimits(Off_or_Def, playersSumm)
    maxs <- scaleMaxMin$maxs
    mins <- scaleMaxMin$mins
  }
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  # NNet model pre-calculated (2 models, Offense and Defense)
  #nn <- .selectedModel(Off_or_Def) 
  if (Off_or_Def == "PTS"){
    nn <- nn_Offense
  } else {
    nn <- nn_Defense  
  }
  
  # Prediction
  scaled <- dplyr::select(scaled, -team_season)
  pr.nn <- compute(nn,scaled)
  #test_pr <- compute(nn,testing[,-ncol(testing)])
  # Model results are scaled so need to re-scale them back to normal
  #pr.nn_ <- pr.nn$net.result*(scaleMaxMin["PTS","maxs"]-scaleMaxMin["PTS","mins"])+scaleMaxMin["PTS","mins"]
  Off_or_Def_teamStats <- select(team_stats, one_of(Off_or_Def))
  pr.nn_ <- pr.nn$net.result*(max(Off_or_Def_teamStats)-min(Off_or_Def_teamStats))+min(Off_or_Def_teamStats)
  pred_PTS <- as.numeric(pr.nn_)
  # bring back the team names
  pr_pts <- cbind(team_season,pred_PTS)
  pr_pts <- as.data.frame(pr_pts)
  
  return(pr_pts)
}

# Put together teams and predicted powers as input to a new regular season
.teamsPredictedPower <- function(data = playersNew, defaultMin = NULL, actualOrPred="actual",
                                 maxs_vector = NULL, mins_vector = NULL) {
  
  Def <- .computePower(data = data,Off_or_Def = "PTSA",thisTeam = "All",defaultMinutes = defaultMin,
                       actualOrPredicted = actualOrPred, maxs_vector = maxs_vector, mins_vector = mins_vector)
  Off <- .computePower(data = data,Off_or_Def = "PTS", thisTeam = "All",defaultMinutes = defaultMin,
                       actualOrPredicted = actualOrPred, maxs_vector = maxs_vector, mins_vector = mins_vector)
  team_power <- merge(Off,Def,by="team_season")
  
  team_power <- team_power %>%
    mutate(teamCode = substr(team_season,1,3),
                       Season = substr(team_season, 5,13))
  
  team_power <- as.data.frame(team_power)
  names(team_power) <- c("team_season","TEAM_PTS","TEAM_PTSAG","TeamCode","Season")
  
  team_power$TEAM_PTS <- as.numeric(as.character(team_power$TEAM_PTS))
  team_power$TEAM_PTSAG <- as.numeric(as.character(team_power$TEAM_PTSAG))
  
  return(team_power)
  
}

# Analytical calculation of wins based on teamsPowers and Normal distribution defined by those powers
.computeWins <- function(data){
  
  # Load season schedule
  season <- realSeasonSchedule %>%
    mutate(Date = paste(Date,StartTime)) %>%
    dplyr::select(-StartTime)
  
  # calculate wins
  wins <- data.frame()
  for (i in 1:nrow(season)){
    thisGame <- .calculateWinProbability(data,season[i,2],season[i,3],home_away_factor)
    wins[i,1] <- thisGame
    wins[i,2] <- 1-thisGame
  }
  return(wins)
}


.teamsPredictedWins <- function(data) {
    
  set.seed(456) 
  # use the actual schedule
  
  
  regSeasonProbs <- .computeWins(data)
  seasonProbs <- bind_cols(realSeasonSchedule,regSeasonProbs)
  names(seasonProbs) <- c("day","time","home_team","away_team","home_team_winProb","away_team_winProb")
  datesRange <- unique(seasonProbs$day)
  
  homeWins <- group_by(seasonProbs, home_team) %>%
    mutate(home_wins = sum(home_team_winProb)) %>%
    select(team = home_team,wins = home_wins) %>%
    distinct()
  awayWins <- group_by(seasonProbs, away_team) %>%
    mutate(away_wins = sum(away_team_winProb)) %>%
    select(team = away_team,wins = away_wins) %>%
    distinct()
  
  seasonWins <- rbind(homeWins,awayWins) %>%
    group_by(team) %>%
    summarise_if(is.numeric,sum) %>%
    as.data.frame()
  
  return(seasonWins) 
  
}

# Compute teams average stats
.computeTeamStats <- function(data = playersNew, thisTeam = "All", actualOrPredicted="predicted", defaultMinutes = NULL,removeEffMin = TRUE) {
  
  if (actualOrPredicted=="actual"){ # whether actual data (before prediction) or predicted data
    playersSumm <- .prepareModelPrediction(data=data, thisTeam=thisTeam)  
  } else {
    playersSumm <- .prepareModelOncePredicted(data_team=data, thisTeam=thisTeam)  
  }
  
  # effMin is 1 of the variables that get averaged weighted by effMin, in case it adds noise to the
  # neural network 
  if (!is.null(defaultMinutes)) { 
    playersSumm$effMin <- defaultMinutes
  }
  if (removeEffMin & ncol(select(playersSumm, one_of("effMin")))>0) { 
    playersSumm <- select(playersSumm, -effMin)
  }
  
  playersSumm <- mutate(playersSumm, Tm = substr(team_season,1,3), Season = substr(team_season,5,nchar(team_season))) %>%
    select(Tm, Season, everything(), -team_season)
  
  return(playersSumm)
  
}
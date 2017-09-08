# Phase 2 ------------------------------------
# Once rosters are updated (Phase 1), predict avg points and avg points against
# per team for the new season or for a season in the past (back to 1979-1980)
# compute avg PTS as offensive power and PTSA as defensive power
.computePower <- function(data = playersNew, Off_or_Def, thisTeam = "All"){
  
  # specifically, this function will prepare playersNew dataset by default
  # It is understood, playersNew is the updated rosters at the beginning of a new season
  playersSumm <- .prepareModelPrediction(data, thisTeam) 
  
  # scale the variables the same way the training dataset was scaled so the nnet makes sense
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  maxs <- scaleMaxMin$maxs[-nrow(scaleMaxMin)] 
  mins <- scaleMaxMin$mins[-nrow(scaleMaxMin)]
  team_season <- playersSumm[,ncol(playersSumm)]
  scaled <- as.data.frame(scale(playersSumm[,-ncol(playersSumm)], center = mins, scale = maxs - mins))
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
  pr.nn_ <- pr.nn$net.result*(scaleMaxMin["PTS","maxs"]-scaleMaxMin["PTS","mins"])+scaleMaxMin["PTS","mins"]
  pred_PTS <- as.numeric(pr.nn_)
  # bring back the team names
  pr_pts <- cbind(team_season,pred_PTS)
  pr_pts <- as.data.frame(pr_pts)
  
  return(pr_pts)
}

# Put together teams and predicted powers as input to a new regular season
.teamsPredictedPower <- function() {
  
  Def <- .computePower("PTSA")
  Off <- .computePower("PTS")
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

# team_power <- .teamsPredictedPower()

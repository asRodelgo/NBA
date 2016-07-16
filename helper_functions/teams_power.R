# Phase 2 ------------------------------------
# Once rosters are updated (Phase 1), predict avg points and avg points against
# per team for the new season or for a season in the past (back to 1979-1980)
# compute avg PTS as offensive power and PTSA as defensive power
.computePower <- function(Off_or_Def){
  
  # updated data for rosters for this season
  playersSumm <- .prepareModelPrediction()
  # scale the variables the same way the training dataset was scaled so the nnet makes sense
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  maxs <- scaleMaxMin$maxs[-nrow(scaleMaxMin)] 
  mins <- scaleMaxMin$mins[-nrow(scaleMaxMin)]
  team_season <- playersSumm[,ncol(playersSumm)]
  scaled <- as.data.frame(scale(playersSumm[,-ncol(playersSumm)], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  # NNet model # TO DO: store this model. No need to recalculate until new data is available
  # 2 models, Offense and Defense
  nn <- .selectedModel(Off_or_Def) 
  # Prediction
  scaled <- dplyr::select(scaled, -team_season)
  pr.nn <- compute(nn,scaled)
  # Model results are scaled so need to re-scale them back to normal
  pr.nn_ <- pr.nn$net.result*(scaleMaxMin["PTS","maxs"]-scaleMaxMin["PTS","mins"])+scaleMaxMin["PTS","mins"]
  pred_PTS <- as.numeric(pr.nn_)
  # bring back the team names
  pr_pts <- cbind(team_season,pred_PTS)
  pr_pts <- as.data.frame(pr_pts)
  
  return(pr_pts)
}

Def <- .computePower("PTSA")
Off <- .computePower("PTS")
team_power <- merge(Off,Def,by="team_season")
team_power <- mutate(team_power, teamCode = substr(team_season,1,3))
Off_act <- filter(team_stats, Season == "2015-2016")[,c("teamCode","PTS")]
Def_act <- filter(team_stats, Season == "2015-2016")[,c("teamCode","PTSA")]
team_power_act <- merge(Off_act,Def_act,by="teamCode")
compare_pred <- merge(team_power,team_power_act,by="teamCode")

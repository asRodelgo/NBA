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
  
}



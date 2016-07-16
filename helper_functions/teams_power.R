# Phase 2 ------------------------------------
# Once rosters are updated (Phase 1), predict avg points and avg points against
# per team for the new season or for a season in the past (back to 1979-1980)
# compute avg PTS as offensive power and PTSA as defensive power
.computePower <- function(Off_or_Def){
  
  # updated data for rosters for this season
  playersSumm <- .prepareModelPrediction()
  # NNet model # TO DO: store this model. No need to recalculate until new data is available
  # 2 models, Offense and Defense
  nn <- .selectedModel(Off_or_Def) 
  # Prediction
  pr.nn <- compute(nn,playersSumm[,-ncol(playersSumm)])
  # Model results are scaled so need to scale them back to normal
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  pr.nn_ <- pr.nn$net.result*(scaleMaxMin["PTS","maxs"]-scaleMaxMin["PTS","mins"])+scaleMaxMin["PTS","mins"]
  
}
# Phase 2 ------------------------------------
# Once rosters are updated (Phase 1), predict avg points and avg points against
# per team for the new season or for a season in the past (back to 1979-1980)
#

.computePower <- function(Off_or_Def){
  
  # updated data for rosters for this season
  playersSumm <- .prepareModelPrediction()
  # NNet model
  nn <- .selectedModel(Off_or_Def) # TODO: store this model. No need to recalculate until next season
  
  
}
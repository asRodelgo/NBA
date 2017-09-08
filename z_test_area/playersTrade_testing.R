# Trade testing area

data <- playersNew
playA <- "Paul George"
playB <- "Kevin Love"
tmA <- "IND"
tmB <- "CLE"
tmA_Power_before_Trade <- merge(.computePower(data,"PTS",tmA),.computePower(data,"PTSA",tmA),by="team_season")
tmB_Power_before_Trade <- merge(.computePower(data,"PTS",tmB),.computePower(data,"PTSA",tmB),by="team_season")
# update rosters after trade happens
newData <- .trade_Players(data, playA,tmA,playB,tmB)
# compute new powers
tmA_Power_after_Trade <- merge(.computePower(newData,"PTS",tmA),.computePower(newData,"PTSA",tmA),by="team_season")
tmB_Power_after_Trade <- merge(.computePower(newData,"PTS",tmB),.computePower(newData,"PTSA",tmB),by="team_season")



### .computePower calls 2 functions internally
## adjust playersNew to eff stats per minute for all or 1 particular team
#preparePredict <- .team_preparePredict(data = newData,thisTeam = "BOS") # prepare_rosters.R
## Prepare input vectors for NNetwork. Weighted average of stats (weights = effMinutes)
#thisTeam_ModelPrediction <- .prepareModelPrediction(data = newData, thisTeam = "BOS") # neural_network.R



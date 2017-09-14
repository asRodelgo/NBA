# Trade testing area

data <- playersNew
playA <- "Paul George"
playB <- c("LeBron James")
tmA <- "IND"
tmB <- "CLE"
effMinutes <- NULL # approx the average of all
tmA_Power_before_Trade <- merge(.computePower(data,"PTS",tmA,effMinutes),.computePower(data,"PTSA",tmA,effMinutes),by="team_season")
tmB_Power_before_Trade <- merge(.computePower(data,"PTS",tmB,effMinutes),.computePower(data,"PTSA",tmB,effMinutes),by="team_season")
# update rosters after trade happens
dataPostTrade <- .trade_Players(data, playA,tmA,playB,tmB)
# compute new powers
tmA_Power_after_Trade <- merge(.computePower(dataPostTrade,"PTS",tmA,effMinutes),.computePower(dataPostTrade,"PTSA",tmA,effMinutes),by="team_season")
tmB_Power_after_Trade <- merge(.computePower(dataPostTrade,"PTS",tmB,effMinutes),.computePower(dataPostTrade,"PTSA",tmB,effMinutes),by="team_season")

## Compute playoff powers
dataPlayoffs <- .adjust_Minutes(data,increment = 0.25, topHeavy = 8)
tmC <- "HOU"
tmC_Power_playoffs <- merge(.computePower(dataPlayoffs,"PTS","All",effMinutes),.computePower(dataPlayoffs,"PTSA","All",effMinutes),by="team_season")
tmC_Power_playoffs

## Average player
avgPlayer <- .calculate_AvgPlayer(data)

## Compute team powers after players stat get predicted
data <- playersNew
dataPredicted <- playersNewPredicted
effMinutes <- NULL # approx the average of all  
tmA <- "All"
tmA_Power_before_Prediction <- merge(.computePower(data,"PTS",tmA,effMinutes),.computePower(data,"PTSA",tmA,effMinutes),by="team_season")
tmA_Power_after_Prediction <- merge(.computePower(dataPredicted,"PTS",tmA,effMinutes,actualOrPredicted = "predicted"),.computePower(dataPredicted,"PTSA",tmA,effMinutes,actualOrPredicted = "predicted"),by="team_season")

# Keep rosters as they were at the end of last season for the purpose of the Abstract
# 1. calculate playersNewPredicted
# 2. Merge with current_rosters into playersNewPredicted_Current
# 3. Complete playersNewPredicted with names not matching in current_rosters. Ex: Tim Hardaway vs. Tim Hardaway 2
# 4. Compute power: .computePower()
# 5. If results as expected, go ahead and simulate Kyrie/Isaiah trade for the abstract and simulate
# multiple iterations of a season to estimate wins for each scenatio
# 6. Create a summary table of the different results.

#1 
playersNewPredicted <- .computePredictedPlayerStats
#2
playersNewPredicted_Current <- .mergePredictedWithCurrent()
#3
playersMatch <- merge(current_rosters,playersNewPredicted, by = "Player", all.x = TRUE) %>%
  distinct(Player)

playersNonMatch <- filter(playersNewPredicted_Current, !(Player %in% playersMatch$Player))
# add the players, irrespective of the team
playersManuallyChanged <- filter(playersNonMatch, Player %in% c(
  "Mike Dunleavy 2","Taurean Waller-Prince","Tim Hardaway 2","Nene Hilario","Glenn Robinson 2",
  "Gary Payton 2","Gerald Henderson 2","Kelly Oubre"
))
# now remove those with several teams:
playersManuallyChanged <- filter(playersManuallyChanged, !(grepl("Dunleavy",Player) & !(Tm =="ATL") ))
# now remove those non-matching from playersNewPredicted
playersNewPredicted_Current <- filter(playersNewPredicted_Current, !is.na(Exp))
# and add the manually changed to match non-matching players
playersNewPredicted_Current <- bind_rows(playersNewPredicted_Current,playersManuallyChanged)
playersNewPredicted_Current <- select(playersNewPredicted_Current, -c(Exp,College))
#4 
teamPowers_after_Prediction <- merge(.computePower(playersNewPredicted_Current,"PTS","All",effMinutes,actualOrPredicted = "predicted"),.computePower(playersNewPredicted_Current,"PTSA","All",effMinutes,actualOrPredicted = "predicted"),by="team_season")
# simulate a season to test this
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Current,actualOrPred="predicted")
# Testing regular season ----------------------------------
regSeasonOutcome <- .standings(real = TRUE)

for (i in 1:10){
  
}
thisTeam <- regSeasonOutcome[[1]][[168]]$team
thisWins <- regSeasonOutcome[[1]][[168]]$win
thisLoses <- regSeasonOutcome[[1]][[168]]$lose

scores <- regSeasonOutcome[[2]]


#5



# prepare Rookie stats
#rookieStats_Prepared <- .team_preparePredict(data = rookieStats, thisTeam = "All",singlePlayer = FALSE)

### .computePower calls 2 functions internally
## adjust playersNew to eff stats per minute for all or 1 particular team
#preparePredict <- .team_preparePredict(data = newData,thisTeam = "BOS") # prepare_rosters.R
## Prepare input vectors for NNetwork. Weighted average of stats (weights = effMinutes)
#thisTeam_ModelPrediction <- .prepareModelPrediction(data = newData, thisTeam = "BOS") # neural_network.R



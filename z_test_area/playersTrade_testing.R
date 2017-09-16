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

# one single season
regSeasonOutcome <- .standings(real = TRUE)


####################################################################
########################## ABSTRACT TABLE ##########################
### Scenario 1: Before Trades
# Starting point: playersNewPredicted_Current
playersNewPredicted_Current_Adj <- .redistributeMinutes(playersNewPredicted_Current,topHeavy = 7, topMinShare = .7)
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Current_Adj,actualOrPred="predicted")
# one single season to start it off
regSeasonOutcome <- .standings(real = TRUE)
# Initialize parameters
regSeasonAvg <- data.frame(
  team = regSeasonOutcome[[1]][[168]]$team,
  teamCode = regSeasonOutcome[[1]][[168]]$teamCode,
  conference = regSeasonOutcome[[1]][[168]]$conference,
  win = 0,
  lose = 0,
  win2 = 0,
  sd = 0,
  probChamp = 0)

num_seasons <- 1000

for (i in 1:num_seasons){
  
  final_standings <- regSeasonOutcome[[1]][[168]]
  #playoffs <- .getPlayoffResults(final_standings) %>% mutate(round = ifelse(round == 0,1,0)) %>%
  #  group_by(teamCode) %>% summarise(round = sum(round)) %>% ungroup()
  regSeasonAvg$win <- regSeasonAvg$win + final_standings$win
  regSeasonAvg$win2 <- regSeasonAvg$win2 + (final_standings$win)^2
  probChamp <- merge(final_standings, playoffs[,c("teamCode","round")],by="teamCode",all.x=TRUE) %>%
    mutate(round = ifelse(is.na(round),0,round))
  #regSeasonAvg$probChamp <- regSeasonAvg$probChamp + probChamp$round
  # generate a new season outcome
  regSeasonOutcome <- .standings(real = TRUE)
  # keep count
  print(paste0("iteration: ",i))
}

regSeasonAvg$win <- regSeasonAvg$win/num_seasons
regSeasonAvg$lose <- 82 - regSeasonAvg$win
regSeasonAvg$win2 <- regSeasonAvg$win2/num_seasons
regSeasonAvg$sd <- sqrt(regSeasonAvg$win2 - (regSeasonAvg$win)^2)
regSeasonAvg$probChamp <- regSeasonAvg$probChamp/num_seasons
write.csv(regSeasonAvg, "data/abstract_regSeasonAvg.csv", row.names = FALSE)

#5
### Scenario 2: Trade: Kyrie for Isaiah
# 5.1 Trade players
# Starting point: playersNewPredicted_Current
playA <- c("Kyrie Irving")
playB <- c("Isaiah Thomas")
tmA <- "CLE"
tmB <- "BOS"
effMinutes <- NULL # approx the average of all
playersNewPredicted_Trade2 <- .trade_Players(playersNewPredicted_Current, playA,tmA,playB,tmB)
# 5.2 Adjust minutes of play. Convert minutes to percent of total time
playersNewPredicted_Trade_Adj2 <- .redistributeMinutes(playersNewPredicted_Trade2,topHeavy = 7, topMinShare = .7)
# 5.3 Recalculate teamsPredicted
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Trade_Adj2,actualOrPred="predicted")
# 5.4 Simulate N seasons
# one single season to start it off
regSeasonOutcome <- .standings(real = TRUE)
# Initialize parameters
regSeasonAvg2 <- data.frame(
  team = regSeasonOutcome[[1]][[168]]$team,
  teamCode = regSeasonOutcome[[1]][[168]]$teamCode,
  conference = regSeasonOutcome[[1]][[168]]$conference,
  win = 0,
  lose = 0,
  win2 = 0,
  sd = 0,
  probChamp = 0)

num_seasons <- 1000

for (i in 1:num_seasons){
  
  final_standings <- regSeasonOutcome[[1]][[168]]
  #playoffs <- .getPlayoffResults(final_standings) %>% mutate(round = ifelse(round == 0,1,0)) %>%
  #  group_by(teamCode) %>% summarise(round = sum(round)) %>% ungroup()
  regSeasonAvg2$win <- regSeasonAvg2$win + final_standings$win
  regSeasonAvg2$win2 <- regSeasonAvg2$win2 + (final_standings$win)^2
  probChamp <- merge(final_standings, playoffs[,c("teamCode","round")],by="teamCode",all.x=TRUE) %>%
    mutate(round = ifelse(is.na(round),0,round))
  #regSeasonAvg2$probChamp <- regSeasonAvg2$probChamp + probChamp$round
  # generate a new season outcome
  regSeasonOutcome <- .standings(real = TRUE)
  # keep count
  print(paste0("iteration: ",i))
}

regSeasonAvg2$win <- regSeasonAvg2$win/num_seasons
regSeasonAvg2$lose <- 82 - regSeasonAvg2$win
regSeasonAvg2$win2 <- regSeasonAvg2$win2/num_seasons
regSeasonAvg2$sd <- sqrt(regSeasonAvg2$win2 - (regSeasonAvg2$win)^2)
regSeasonAvg2$probChamp <- regSeasonAvg2$probChamp/num_seasons
write.csv(regSeasonAvg2, "data/abstract_regSeasonAvg_Kyrie_Isaiah.csv", row.names = FALSE)

### Scenario 3: Trade: Kyrie for Isaiah + Jae Crowder
# 5.1 Trade players
# Starting point: playersNewPredicted_Current
playA <- c("Kyrie Irving")
playB <- c("Isaiah Thomas", "Jae Crowder")
tmA <- "CLE"
tmB <- "BOS"
effMinutes <- NULL # approx the average of all
playersNewPredicted_Trade2 <- .trade_Players(playersNewPredicted_Current, playA,tmA,playB,tmB)
# 5.2 Adjust minutes of play. Convert minutes to percent of total time
playersNewPredicted_Trade_Adj2 <- .redistributeMinutes(playersNewPredicted_Trade2,topHeavy = 7, topMinShare = .7)
# 5.3 Recalculate teamsPredicted
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Trade_Adj2,actualOrPred="predicted")
# 5.4 Simulate N seasons
# one single season to start it off
regSeasonOutcome <- .standings(real = TRUE)
# Initialize parameters
regSeasonAvg2 <- data.frame(
  team = regSeasonOutcome[[1]][[168]]$team,
  teamCode = regSeasonOutcome[[1]][[168]]$teamCode,
  conference = regSeasonOutcome[[1]][[168]]$conference,
  win = 0,
  lose = 0,
  win2 = 0,
  sd = 0,
  probChamp = 0)

num_seasons <- 1000

for (i in 1:num_seasons){
  
  final_standings <- regSeasonOutcome[[1]][[168]]
  #playoffs <- .getPlayoffResults(final_standings) %>% mutate(round = ifelse(round == 0,1,0)) %>%
  #  group_by(teamCode) %>% summarise(round = sum(round)) %>% ungroup()
  regSeasonAvg2$win <- regSeasonAvg2$win + final_standings$win
  regSeasonAvg2$win2 <- regSeasonAvg2$win2 + (final_standings$win)^2
  probChamp <- merge(final_standings, playoffs[,c("teamCode","round")],by="teamCode",all.x=TRUE) %>%
    mutate(round = ifelse(is.na(round),0,round))
  #regSeasonAvg2$probChamp <- regSeasonAvg2$probChamp + probChamp$round
  # generate a new season outcome
  regSeasonOutcome <- .standings(real = TRUE)
  # keep count
  print(paste0("iteration: ",i))
}

regSeasonAvg2$win <- regSeasonAvg2$win/num_seasons
regSeasonAvg2$lose <- 82 - regSeasonAvg2$win
regSeasonAvg2$win2 <- regSeasonAvg2$win2/num_seasons
regSeasonAvg2$sd <- sqrt(regSeasonAvg2$win2 - (regSeasonAvg2$win)^2)
regSeasonAvg2$probChamp <- regSeasonAvg2$probChamp/num_seasons
write.csv(regSeasonAvg2, "data/abstract_regSeasonAvg_Kyrie_Isaiah_Jae.csv", row.names = FALSE)


### Scenario 4: Trade: Kyrie for Isaiah + Jae Crowder. Isaiah can't play due to hip injury
# 5.1 Trade players
# Starting point: playersNewPredicted_Current
playA <- c("Kyrie Irving")
playB <- c("Jae Crowder")
tmA <- "CLE"
tmB <- "BOS"
effMinutes <- NULL # approx the average of all
playersNewPredicted_Trade2 <- .trade_Players(playersNewPredicted_Current, playA,tmA,playB,tmB)
# 5.2 Adjust minutes of play. Convert minutes to percent of total time
playersNewPredicted_Trade_Adj2 <- .redistributeMinutes(playersNewPredicted_Trade2,topHeavy = 7, topMinShare = .7)
# 5.3 Recalculate teamsPredicted
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Trade_Adj2,actualOrPred="predicted")
# 5.4 Simulate N seasons
# one single season to start it off
regSeasonOutcome <- .standings(real = TRUE)
# Initialize parameters
regSeasonAvg2 <- data.frame(
  team = regSeasonOutcome[[1]][[168]]$team,
  teamCode = regSeasonOutcome[[1]][[168]]$teamCode,
  conference = regSeasonOutcome[[1]][[168]]$conference,
  win = 0,
  lose = 0,
  win2 = 0,
  sd = 0,
  probChamp = 0)

num_seasons <- 1000

for (i in 1:num_seasons){
  
  final_standings <- regSeasonOutcome[[1]][[168]]
  #playoffs <- .getPlayoffResults(final_standings) %>% mutate(round = ifelse(round == 0,1,0)) %>%
  #  group_by(teamCode) %>% summarise(round = sum(round)) %>% ungroup()
  regSeasonAvg2$win <- regSeasonAvg2$win + final_standings$win
  regSeasonAvg2$win2 <- regSeasonAvg2$win2 + (final_standings$win)^2
  probChamp <- merge(final_standings, playoffs[,c("teamCode","round")],by="teamCode",all.x=TRUE) %>%
    mutate(round = ifelse(is.na(round),0,round))
  #regSeasonAvg2$probChamp <- regSeasonAvg2$probChamp + probChamp$round
  # generate a new season outcome
  regSeasonOutcome <- .standings(real = TRUE)
  # keep count
  print(paste0("iteration: ",i))
}

regSeasonAvg2$win <- regSeasonAvg2$win/num_seasons
regSeasonAvg2$lose <- 82 - regSeasonAvg2$win
regSeasonAvg2$win2 <- regSeasonAvg2$win2/num_seasons
regSeasonAvg2$sd <- sqrt(regSeasonAvg2$win2 - (regSeasonAvg2$win)^2)
regSeasonAvg2$probChamp <- regSeasonAvg2$probChamp/num_seasons
write.csv(regSeasonAvg2, "data/abstract_regSeasonAvg_Kyrie_Jae.csv", row.names = FALSE)

# prepare Rookie stats
#rookieStats_Prepared <- .team_preparePredict(data = rookieStats, thisTeam = "All",singlePlayer = FALSE)

### .computePower calls 2 functions internally
## adjust playersNew to eff stats per minute for all or 1 particular team
#preparePredict <- .team_preparePredict(data = newData,thisTeam = "BOS") # prepare_rosters.R
## Prepare input vectors for NNetwork. Weighted average of stats (weights = effMinutes)
#thisTeam_ModelPrediction <- .prepareModelPrediction(data = newData, thisTeam = "BOS") # neural_network.R



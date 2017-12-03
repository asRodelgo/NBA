thisGSW <- filter(playersPredictedStats_adjMin, Tm == "GSW")

new_playersAdjMin <- 
  .trade_Players(data=playersPredictedStats_adjMin,playA="Isaiah Thomas",tmA=NULL, 
                 playB=NULL, tmB=NULL) %>% arrange(Player)

new_playersAdjMin <- playersPredictedStats_adjMin

thisGSW_afterIsaiah <- filter(new_playersAdjMin, Tm == "GSW")
######

playersPredictedStats_adjMin2 <- .redistributeMinutes(data = new_playersAdjMin, topHeavy = 7, topMinShare = .6, min_share_top1 = .105)
playersPredictedStats_adjMin2 <- mutate(playersPredictedStats_adjMin2,Season = paste0(thisYear,"-",as.numeric(thisYear)+1)) %>% select(-Pick, -Exp) %>% as.data.frame()
playersPredictedStats_adjPer <- group_by(playersPredictedStats_adjMin2, Tm) %>% mutate(effMin = effMin/sum(effMin,na.rm=TRUE)) %>% as.data.frame()

#source("code_chunks/source_computeOffenseDefense.R",local=TRUE)
# compute teams and players Offense and Defense
playersNewPredicted_Final_adjMinPer2 <- select(playersPredictedStats_adjPer, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
playersNewPredicted_OffDef <- mutate(playersNewPredicted_Final_adjMinPer2, Tm = Player, effMin = 1)
playersPredicted <- .teamsPredictedPower(data = playersNewPredicted_OffDef,actualOrPred="predicted") %>%
  mutate(Player = substr(team_season,1,regexpr("_",team_season)-1),plusMinus = TEAM_PTS-TEAM_PTSAG) %>%
  select(Player,Offense = TEAM_PTS, Defense = TEAM_PTSAG, plusMinus) %>%
  as.data.frame()

playersPredicted2 <- merge(playersPredicted,new_playersAdjMin[,c("Player","Exp","Age","Tm","effMin")], by ="Player") %>% 
  mutate(adjPlusMinus = plusMinus*effMin*100) %>% 
  group_by(Tm) %>% 
  mutate(teamPlusMinus = sum(adjPlusMinus,na.rm=TRUE)) %>% 
  ungroup()

playersDatabase <- merge(playersPredictedStats_adjPer,select(playersPredicted2, -c(Age,Tm,effMin)), by = "Player")

playersRanks <- mutate_if(playersDatabase, is.numeric, function(x) row_number(desc(x)))
playerMax <- summarise_if(playersDatabase, is.numeric, max)
playerMin <- group_by(playersDatabase, Player) %>% summarise_if(is.numeric, min)

#source("code_chunks/source_predictedPowersWins.R",local=TRUE)
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Final_adjMinPer2, 
                                       actualOrPred="predicted", maxs_vector = maxs_vector_input,
                                       mins_vector = mins_vector_input)
win_predictions <- .teamsPredictedWins(data = teamsPredicted)






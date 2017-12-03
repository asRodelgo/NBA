# compute teams and players Offense and Defense
playersNewPredicted_Final_adjMinPer2 <- select(playersPredictedStats_adjPer, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
playersNewPredicted_OffDef <- mutate(playersNewPredicted_Final_adjMinPer2, Tm = Player, effMin = 1)
playersPredicted <- .teamsPredictedPower(data = playersNewPredicted_OffDef,actualOrPred="predicted",maxs_vector = maxs_vector_input,
                                         mins_vector = mins_vector_input) %>%
  mutate(Player = substr(team_season,1,regexpr("_",team_season)-1),plusMinus = TEAM_PTS-TEAM_PTSAG) %>%
  select(Player,Offense = TEAM_PTS, Defense = TEAM_PTSAG, plusMinus) %>%
  as.data.frame()

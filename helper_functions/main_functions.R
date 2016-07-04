# Season scores --------

calculateScore <- function(team_home,team_away){
  
  # Single game simulation ----------------
  #team_home <- "PHI"
  #team_away <- "BKN"
  
  teamH <- filter(teams, TeamCode == team_home)
  teamA <- filter(teams, TeamCode == team_away)
  
  muH <- teamH$TEAM_PTS + 3 + teamA$TEAM_PTSAG - global_mean
  muA <- teamA$TEAM_PTS - 3 + teamH$TEAM_PTSAG - global_mean
  
  pointsH <- round(rnorm(1,muH,sigma),0)
  pointsA <- round(rnorm(1,muA,sigma),0)
  
  numOT <- 0
  while (abs(pointsH-pointsA)<1){ # overtime tie-breaker
    extraH <- round(rnorm(1,muH*5/48,sigma/3),0)
    extraA <- round(rnorm(1,muA*5/48,sigma/3),0)
    pointsH <- pointsH + extraH
    pointsA <- pointsA + extraA
    numOT <- numOT + 1
  }
  #print(paste0(team_home,": ",pointsH," vs. ",team_away,": ",pointsA))
  return(c(pointsH,pointsA,numOT))
}

computeScores <- function(){
  
  scores <- data.frame()
  for (i in 1:nrow(season)){
    thisGame <- calculateScore(season[i,2],season[i,3])
    scores[i,1] <- thisGame[1]
    scores[i,2] <- thisGame[2]
    scores[i,3] <- thisGame[3]
  }
  return(scores)
}
  
regSeasonScores <- computeScores()

season <- bind_cols(season,regSeasonScores)
names(season) <- c("day","home_team","away_team","home_points","away_points","numOT")

homeWins <- season %>%
  group_by(home_team) %>%
  mutate(home_wins = sum(home_points-away_points>0))

awayWins <- homeWins %>%
  group_by(away_team) %>%
  mutate(away_wins = sum(away_points-home_points>0))

homeWins <- distinct(homeWins,home_team,.keep_all = TRUE)
homeWins <- select(homeWins, home_team,home_wins)
homeWins <- as.data.frame(homeWins)

awayWins <- distinct(awayWins,away_team,.keep_all = TRUE)
awayWins <- select(awayWins, away_team,away_wins)
awayWins <- as.data.frame(awayWins)

totalWins <- merge(homeWins,awayWins, by.x="home_team",by.y="away_team")
totalWins <- mutate(totalWins, win = home_wins + away_wins, lose = 82-win)
totalWins <- arrange(totalWins,desc(win))




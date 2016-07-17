# Season scores --------
# compute score between any 2 teams during regular season
.calculateScore <- function(team_home,team_away){
  
  # Single game simulation ----------------
  #team_home <- "PHI"
  #team_away <- "BOS"
  # ---------------------------------------
  
  # teamsPredicted contain predicted avg PTS and avg PTS Against per team for a new season
  teamH <- filter(teamsPredicted, TeamCode == team_home)
  teamA <- filter(teamsPredicted, TeamCode == team_away)
  
  # Define both Normal distributions. Empirical home-away difference is 6 points (+3, -3)
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

.computeScores <- function(){
  
  season <- seasonSchedule # Load season schedule
  
  # calculate all scores
  scores <- data.frame()
  for (i in 1:nrow(season)){
    thisGame <- .calculateScore(season[i,2],season[i,3])
    scores[i,1] <- thisGame[1]
    scores[i,2] <- thisGame[2]
    scores[i,3] <- thisGame[3]
  }
  return(scores)
}

.standings <- function() {

  # compute all scores for regular season  
  regSeasonScores <- .computeScores()
  
  season <- bind_cols(seasonSchedule,regSeasonScores)
  names(season) <- c("day","home_team","away_team","home_points","away_points","numOT")
  
  # compute standings by day for regular season
  standings <- list() # standings is a list in which each day of competition is a data.frame
  standings_aux <- data.frame(team = conferences$Team, teamCode = conferences$TeamCode,
                               conference = conferences$Conference, win = 0, lose = 0,
                               win_home = 0, win_conf = 0, streak = 0)
  for (i in 1:tail(season,1)$day){
    
    thisDay <- filter(season,day == i)
    for (j in 1:nrow(thisDay)){
      
      HT <- standings_aux[standings_aux$teamCode==thisDay$home_team[j],]
      AT <- standings_aux[standings_aux$teamCode==thisDay$away_team[j],]
      
      if (thisDay$home_points[j] > thisDay$away_points[j]){ # home team wins
        HT$win <- HT$win + 1
        AT$lose <- AT$lose + 1
        HT$win_home <- HT$win_home + 1
        HT$win_conf <- ifelse(HT$conference==AT$conference,HT$win_conf + 1,HT$win_conf)
        HT$streak <- ifelse(HT$streak <= 0,1,HT$streak + 1)
        AT$streak <- ifelse(AT$streak >= 0,-1,AT$streak - 1)
        
      } else { # away team wins
        AT$win <- AT$win + 1
        HT$lose <- HT$lose + 1
        AT$win_conf <- ifelse(AT$conference==HT$conference,AT$win_conf + 1,AT$win_conf)
        AT$streak <- ifelse(AT$streak <= 0,1,AT$streak + 1)
        HT$streak <- ifelse(HT$streak >= 0,-1,HT$streak - 1)
      }
      standings_aux[standings_aux$teamCode==thisDay$home_team[j],] <- HT
      standings_aux[standings_aux$teamCode==thisDay$away_team[j],] <- AT
    }
    
    standings[[i]] <- standings_aux
    
  }
  return(standings)

}

standings <- .standings()
x <- 370
arrange(filter(standings[[x]], conference == "E"), desc(win/(win+lose)))
arrange(filter(standings[[x]], conference == "W"), desc(win/(win+lose)))

# 
# homeWins <- season %>%
#   group_by(home_team) %>%
#   mutate(home_wins = sum(home_points-away_points>0))
# 
# awayWins <- homeWins %>%
#   group_by(away_team) %>%
#   mutate(away_wins = sum(away_points-home_points>0))
# 
# homeWins <- distinct(homeWins,home_team,.keep_all = TRUE)
# homeWins <- select(homeWins, home_team,home_wins)
# homeWins <- as.data.frame(homeWins)
# 
# awayWins <- distinct(awayWins,away_team,.keep_all = TRUE)
# awayWins <- select(awayWins, away_team,away_wins)
# awayWins <- as.data.frame(awayWins)
# 
# totalWins <- merge(homeWins,awayWins, by.x="home_team",by.y="away_team")
# totalWins <- mutate(totalWins, win = home_wins + away_wins, lose = 82-win)
# totalWins <- arrange(totalWins,desc(win))
# 
# 


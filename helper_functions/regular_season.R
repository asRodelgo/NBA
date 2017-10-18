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
  
  # Define both Normal distributions. Empirical home-away difference is approx (2*home_away_factor) 6 points (+3, -3)
  muH <- teamH$TEAM_PTS + home_away_factor/2 + teamA$TEAM_PTSAG - global_mean
  muA <- teamA$TEAM_PTS - home_away_factor/2 + teamH$TEAM_PTSAG - global_mean
  
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

.calculateWinProbability <- function(team_home,team_away,home_away_factor){
  
  # teamsPredicted contain predicted avg PTS and avg PTS Against per team for a new season
  teamH <- filter(teamsPredicted, TeamCode == team_home)
  teamA <- filter(teamsPredicted, TeamCode == team_away)
  
  # Define both Normal distributions. Empirical home-away difference is approx (2*home_away_factor) 6 points (+3, -3)
  muH <- teamH$TEAM_PTS + home_away_factor/2 + teamA$TEAM_PTSAG - global_mean
  muA <- teamA$TEAM_PTS - home_away_factor/2 + teamH$TEAM_PTSAG - global_mean
  
  prob_HvsA <- 1-pnorm(0,muH-muA,sqrt(2*sigma))
  # equivalent simulated probability (to double check analytical probability)
  # prob_HvsA_sim <- length(which(rnorm(100000,muH-muA,sqrt(2*sigma))>0))/100000
  
  return(prob_HvsA)
}


.computeScores <- function(real=FALSE){
  
  # Load season schedule
  if (real){
    season <- realSeasonSchedule %>%
      mutate(Date = paste(Date,StartTime)) %>%
      dplyr::select(-StartTime)
    
  } else {
    season <- seasonSchedule
  }
    
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

.standings <- function(real=FALSE) {

  set.seed(as.integer(Sys.time())) # always a different seed
  # compute all scores for regular season  
  
  if (real){
    regSeasonScores <- .computeScores(real=TRUE)
    season <- bind_cols(realSeasonSchedule,regSeasonScores)
    names(season) <- c("day","time","home_team","away_team","home_points","away_points","numOT")
    datesRange <- unique(season$day)
  } else {
    regSeasonScores <- .computeScores()
    seasonSchedule <- .seasonSchedule()
    season <- bind_cols(seasonSchedule,regSeasonScores)
    names(season) <- c("day","home_team","away_team","home_points","away_points","numOT")
    datesRange <- c(1:tail(season,1)$day)
  }
  
  
  
  # compute standings by day for regular season
  the_standings <- list() # standings is a list in which each day of competition is a data.frame
  standings_aux <- data.frame(team = conferences$Team, teamCode = conferences$TeamCode,
                              conference = conferences$Conference, win = 0, lose = 0,
                              win_home = 0, lose_home = 0, win_home_perc = 0, 
                              win_conf = 0, lose_conf = 0, win_conf_perc = 0, 
                              tot_pts = 0, avg_pts = 0, tot_pts_ag = 0, avg_pts_ag = 0, 
                              streak = 0)
  
  for (i in datesRange){
    
    thisDay <- filter(season,day == i)
    for (j in 1:nrow(thisDay)){
      
      HT <- standings_aux[standings_aux$teamCode==thisDay$home_team[j],]
      AT <- standings_aux[standings_aux$teamCode==thisDay$away_team[j],]
      
      if (thisDay$home_points[j] > thisDay$away_points[j]){ # home team wins
        HT$win <- HT$win + 1
        AT$lose <- AT$lose + 1
        HT$win_home <- HT$win_home + 1
        HT$win_home_perc <- round(HT$win_home/(HT$win_home + HT$lose_home),2)
        HT$win_conf <- ifelse(HT$conference==AT$conference,HT$win_conf + 1,HT$win_conf)
        AT$lose_conf <- ifelse(HT$conference==AT$conference,AT$lose_conf + 1,AT$lose_conf)
        HT$win_conf_perc <- round(HT$win_conf/(HT$win_conf + HT$lose_conf),2)
        HT$streak <- ifelse(HT$streak <= 0,1,HT$streak + 1)
        AT$streak <- ifelse(AT$streak >= 0,-1,AT$streak - 1)
        
      } else { # away team wins
        AT$win <- AT$win + 1
        HT$lose <- HT$lose + 1
        HT$lose_home <- HT$lose_home + 1
        AT$win_home_perc <- round(AT$win_home/(AT$win_home + AT$lose_home),2)
        AT$win_conf <- ifelse(AT$conference==HT$conference,AT$win_conf + 1,AT$win_conf)
        HT$lose_conf <- ifelse(HT$conference==AT$conference,HT$lose_conf + 1,HT$lose_conf)
        AT$win_conf_perc <- round(AT$win_conf/(AT$win_conf + AT$lose_conf),2)
        AT$streak <- ifelse(AT$streak <= 0,1,AT$streak + 1)
        HT$streak <- ifelse(HT$streak >= 0,-1,HT$streak - 1)
      }
      # points don't depend on outcome of game
      HT$tot_pts <- HT$tot_pts + thisDay$home_points[j]
      HT$tot_pts_ag <- HT$tot_pts_ag + thisDay$away_points[j]
      HT$avg_pts <- round(HT$tot_pts/(HT$win + HT$lose),1)
      HT$avg_pts_ag <- round(HT$tot_pts_ag/(HT$win + HT$lose),1)
      AT$tot_pts <- AT$tot_pts + thisDay$away_points[j]
      AT$tot_pts_ag <- AT$tot_pts_ag + thisDay$home_points[j]
      AT$avg_pts <- round(AT$tot_pts/(AT$win + AT$lose),1)
      AT$avg_pts_ag <- round(AT$tot_pts_ag/(AT$win + AT$lose),1)
      
      standings_aux[standings_aux$teamCode==thisDay$home_team[j],] <- HT
      standings_aux[standings_aux$teamCode==thisDay$away_team[j],] <- AT
    }
    
    the_standings[[i]] <- standings_aux
    
  }
  return(list(the_standings,season)) # list of standings (list) and reg season scores (data.frame)

}


.getConferenceStandings <- function(conf,day){
  
  standings <- regSeasonOutcome[[1]]
  #day <- length(standings)
  confPredStandings <- arrange(filter(dplyr::select(standings[[day]], conference, team,W=win,L=lose,`%W Home`=win_home_perc,`%W Conf`=win_conf_perc,
                                                    PTS=avg_pts,PTSA=avg_pts_ag,Strk=streak), conference == conf), desc(W/(W+L)))
  confPredStandings <- dplyr::select(confPredStandings,-conference)
  
  return(confPredStandings)
}

.getGames <- function(conf,this_day){
  
  games <- regSeasonOutcome[[2]]
  #day <- length(standings)
  confPredGames <- dplyr::select(filter(games,day==this_day), away_team,home_team,
                                         away_points,home_points) %>%
    mutate(game = paste0(away_team," @ ",home_team))
  confPredGames <- dplyr::select(confPredGames,game,A=away_points,H=home_points)
  
  return(confPredGames)
}


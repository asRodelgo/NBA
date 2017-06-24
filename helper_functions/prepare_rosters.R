# ----------------------------
# Calculations by team
# thisTeam <- "GSW"
#thisTeamPlayers <- as.character(filter(playersHist, Tm == thisTeam, Season == max(as.character(Season)))$Player)

# compute effective stats for most current season
.team_prepare <- function(thisTeam="All"){
  
  if (thisTeam == "All"){
    
    data_team <- playersHist %>%
      filter(Season == max(as.character(Season))) %>%
      group_by(Player) %>%
      mutate(effMin = MP/3936, effFG = FG/(3936*effMin),
             effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
             eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
             effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
             effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
             effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
             effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
             effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
             effPTS = PTS/(3936*effMin)) %>%
      dplyr::select(Player,Pos,Season,Tm,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
             FTPer = FT., starts_with("eff"),
             -G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
             -BLK,-TOV,-PF,-FT,-STL,-PTS)
    
    # Impute NAs by 0. If NA means no shot attempted, ie, 
    # either the player didn't play enough time or is really bad at this particular type of shot.
    for (i in 5:ncol(data_team)){
      data_team[is.na(data_team[,i]),i] <- 0
    }
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    data_team <- data_team %>%
      group_by(Tm) %>%
      mutate(effMin = 5*effMin/sum(effMin,na.rm=TRUE))
    
    data_team <- as.data.frame(data_team)
    
  } else{
    data_team <- playersHist %>%
      filter(Tm == thisTeam, Season == max(as.character(Season))) %>%
      group_by(Player) %>%
      mutate(effMin = MP/3936, effFG = FG/(3936*effMin),
             effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
             eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
             effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
             effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
             effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
             effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
             effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
             effPTS = PTS/(3936*effMin)) %>%
      select(Player,Pos,Season,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
             FTPer = FT., starts_with("eff"),
             -Tm,-G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
             -BLK,-TOV,-PF,-FT,-STL,-PTS)
    
    # Impute NAs by 0. If NA means no shot attempted, ie, 
    # either the player didn't play enough time or is really bad at this particular type of shot.
    for (i in 4:ncol(data_team)){
      data_team[is.na(data_team[,i]),i] <- 0
    }
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    teamMinutes <- sum(data_team$effMin)
    data_team$effMin <- 5*data_team$effMin/teamMinutes
    
    data_team <- as.data.frame(data_team)
    
  }
  return(data_team)
}

# compute effective stats for one new season
.team_preparePredict <- function(thisTeam="All"){
  
  if (thisTeam == "All"){
    
    data_team <- playersNew %>%
      group_by(Player) %>%
      mutate(effMin = MP/3936, effFG = FG/(3936*effMin),
             effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
             eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
             effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
             effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
             effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
             effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
             effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
             effPTS = PTS/(3936*effMin)) %>%
      dplyr::select(Player,Pos,Season,Tm,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                    FTPer = FT., starts_with("eff"),
                    -G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                    -BLK,-TOV,-PF,-FT,-STL,-PTS)
    
    # Impute NAs by 0. If NA means no shot attempted, ie, 
    # either the player didn't play enough time or is really bad at this particular type of shot.
    for (i in 5:ncol(data_team)){
      data_team[is.na(data_team[,i]),i] <- 0
    }
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    data_team <- data_team %>%
      group_by(Tm) %>%
      mutate(effMin = 5*effMin/sum(effMin,na.rm=TRUE))
    
    data_team <- as.data.frame(data_team)
    
  } else{
    data_team <- playersNew %>%
      filter(Tm == thisTeam) %>%
      group_by(Player) %>%
      mutate(effMin = MP/3936, effFG = FG/(3936*effMin),
             effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
             eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
             effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
             effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
             effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
             effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
             effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
             effPTS = PTS/(3936*effMin)) %>%
      select(Player,Pos,Season,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
             FTPer = FT., starts_with("eff"),
             -Tm,-G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
             -BLK,-TOV,-PF,-FT,-STL,-PTS)
    
    # Impute NAs by 0. If NA means no shot attempted, ie, 
    # either the player didn't play enough time or is really bad at this particular type of shot.
    for (i in 4:ncol(data_team)){
      data_team[is.na(data_team[,i]),i] <- 0
    }
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    teamMinutes <- sum(data_team$effMin)
    data_team$effMin <- 5*data_team$effMin/teamMinutes
    
    data_team <- as.data.frame(data_team)
    
  }
  return(data_team)
}

# compute effective stats for all seasons
.team_prepareAll <- function(){
  
  data_team <- playersHist %>%
    group_by(Player,Season) %>%
    mutate(effMin = MP/3936, effFG = FG/(3936*effMin),
           effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
           eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
           effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
           effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
           effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
           effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
           effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
           effPTS = PTS/(3936*effMin)) %>%
    dplyr::select(Player,Pos,Season,Tm,Age,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                  FTPer = FT., starts_with("eff"),
                  -G,-GS,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                  -BLK,-TOV,-PF,-FT,-STL,-PTS)
  
  # Impute NAs by 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  for (i in 5:ncol(data_team)){
    data_team[is.na(data_team[,i]),i] <- 0
  }
  # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
  data_team <- data_team %>%
    group_by(Tm,Season) %>%
    mutate(effMin = 5*effMin/sum(effMin,na.rm=TRUE))
  
  data_team <- as.data.frame(data_team)
    
  return(data_team)
}

# swap 2 players (transfer)
.transfer2Players <- function(player1, player2, thisTeam) {
# player1 <- "Harrison Barnes"
# player2 <- "Kevin Durant"
  data_team <- .team_prepare(thisTeam)
  originTeam <- as.character(filter(playersHist, Player == player2, Season == max(as.character(Season)))$Tm)
  player2Stats <- filter(.team_prepare(originTeam), Player == player2)
  data_team <- filter(data_team, !(Player == player1))
  data_team <- bind_rows(data_team, player2Stats)
  # Recalculate stats thisTeam
  teamMinutes <- sum(data_team$effMin)
  data_team$effMin <- 5*data_team$effMin/teamMinutes
  
  data_team <- as.data.frame(data_team)
  return(data_team)

}



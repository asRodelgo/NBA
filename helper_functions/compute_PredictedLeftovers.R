# when all players have been updated and predicted, only leftovers remain (players who were NBA at some point
# and return after 1 or more absent seasons)
.computePredictedPlayerStats_Leftovers <- function(thesePlayers) {
  
  # update currentRosters, europePlayers and College players from write_rookiesDraft.R
  #current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
  #rookies <- read.csv("data/rookies.csv",stringsAsFactors = FALSE)
  #collegePlayers <- read.csv("data/collegePlayers.csv", stringsAsFactors = FALSE)
  #rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  #europePlayers <- read.csv("data/europePlayers.csv", stringsAsFactors = FALSE)
  playersNew <- playersHist %>%
    filter(Season == max(as.character(Season))) %>%
    mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
  
  thesePlayersPredicted <- data.frame()
  for (team in unique(thesePlayers$Tm)){
    #for (team in c("CLE")){
    #thisTeam <- filter(current_rosters, Tm == team)
    thisTeam <- filter(thesePlayers, Tm == team)
    thisTeamStats <- data.frame()
    for (player in thisTeam$Player){
      #if (!(player %in% thesePlayersPredicted$Player)){ # skip running all. Start over where it failed
      thisPlayer <- filter(thisTeam, Player == player)
      print(paste0("Team: ", team,": Processing ",thisPlayer$Player))
      #if (thisPlayer$Exp %in% seq(1,25,1)){ # not a rookie
      if (thisPlayer$Age < 20) { # not enough players to compare to at age 19 or younger
        thisPlayer$Age <- 20
      }
      if (thisPlayer$Age > 39) { # not enough players to compare to at age 41 or older
        thisPlayer$Age <- 39
      }
      thisPlayerStats <- .predictPlayer(thisPlayer$Player,20,thisPlayer$Age,10) %>% 
        select(Player,Pos,Season,Age,everything())
      
      if (nrow(thisPlayerStats)>0){ # in case thisPlayerStats return a non empty data.frame
        if (!is.na(thisPlayerStats$effPTS)){ # rosters not yet updated so include R (last season rookies)
          #if (thisPlayer$Exp %in% c(seq(1,25,1),"R")){ # rosters not yet updated so include R (last season rookies)
          print("NBA player: OK!")
          print(thisPlayerStats)
          
        } else if (player %in% playersHist$Player) { # NBA player that didn't play last season so I look him up in historical seasons
          thisPlayerStats <- .team_preparePredict(filter(playersHist, Player == player))  %>%
            mutate(Age = thisPlayer$Age) %>%
            select(Player,Pos,Season,Age,everything())
          
          print("NBA player: Empty predicted stats!")
          print(thisPlayerStats)
        } else { # compute rookie player average stats for this player
          thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age) %>%
            mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                   G = 10, GS = 0, Tm = team) 
          thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
          print("Average player: OK!")
          print(thisPlayerStats)
        }
      } else {
        thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age) %>%
          mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                 G = 10, GS = 0, Tm = team) 
        thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
        #MP = as.numeric(thisPlayer$MP))
        print("Average player: OK!")
        print(thisPlayerStats)
      }  
      
      if (nrow(thisTeamStats)>0){
        thisTeamStats <- bind_rows(thisTeamStats,thisPlayerStats)
      } else{
        thisTeamStats <- thisPlayerStats
      }
    }
    #}
    if (nrow(thisTeamStats) > 0) {
      thisTeamStats <- mutate(thisTeamStats, Tm = team)
      if (nrow(thesePlayersPredicted)>0){
        thesePlayersPredicted <- bind_rows(thesePlayersPredicted,thisTeamStats)
      } else{
        thesePlayersPredicted <- thisTeamStats
      }
    }
    
    
  }
  thesePlayersPredicted <- distinct(thesePlayersPredicted, Player, Tm, .keep_all=TRUE)
  limitMinutes <- .005
  defaultMinutes <- .001 # assign low minutes to outliers as they most likely belong to players with very little playing time
  thesePlayersPredicted2 <- mutate(thesePlayersPredicted,effMin = ifelse(effMin > limitMinutes, defaultMinutes,effMin))
  #write.csv(thesePlayersPredicted, "data/thesePlayersPredicted.csv", row.names = FALSE)
  return(thesePlayersPredicted2)
} 


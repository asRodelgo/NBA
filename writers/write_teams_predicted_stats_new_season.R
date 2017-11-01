# Write team predicted stats new season --------
# 1. Read final team rosters from basketball reference
# 2. For each team, each player, predict their numbers for the upcoming season:
# 2.1. Non-rookies, use the predict_player functions
# 2.2. Rookies, use college or european stats.
# 2.3. Leftovers, average out from rookie players of similar age in history

# 1
.getLatestRosters <- function(thisSeason = substr(Sys.Date(),1,4), previousSeason = FALSE){
  
  library(httr)
  new_rosters <- data.frame()
  thisSeason <- as.numeric(thisSeason) + 1
  
  current_rosters <- data.frame()
  playersNew <- playersHist %>% # keep only players last season
    filter(Season == max(as.character(Season))) %>%
    mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
  playersNew <- filter(playersNew,!(Tm == "TOT"))
  for (thisTeam in unique(playersNew$Tm)){
    
    url <- paste0("https://www.basketball-reference.com/teams/",thisTeam,"/",thisSeason,".html")
    if (status_code(GET(url)) == 200){ # successful response
      getRoster <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="roster"]') %>%
        html_table(fill = TRUE)
      thisRoster <- getRoster[[1]] %>% select(-`No.`)
      names(thisRoster)[which(names(thisRoster)=='')] <- "Nationality"
      thisRoster <- mutate(thisRoster, Tm = thisTeam, Exp = as.character(Exp))  
      if (nrow(current_rosters)>0){
        current_rosters <- bind_rows(current_rosters,thisRoster)
      } else{
        current_rosters <- thisRoster
      }
    }
  }
  names(current_rosters) <- gsub(" ","_",names(current_rosters))
  #names(current_rosters) <- c(names(current_rosters)[1:5],"Birth_Date","Nationality","Experience","College","Team")
  # I need to compute their current ages for the prediction model is based on their age
  current_rosters <- mutate(current_rosters, Age = thisSeason - as.numeric(substr(Birth_Date,nchar(Birth_Date)-3,nchar(Birth_Date))),
                            Season = paste0(thisSeason-1,"-",thisSeason))
  
  # write current_rosters or rostersLastSeason depending on value of thisSeason
  if (previousSeason) {
    write.csv(current_rosters, "data/rostersLastSeason.csv",row.names = FALSE)
  } else {
    write.csv(current_rosters, "data/currentRosters.csv",row.names = FALSE)
  }

}

.computePredictedPlayerStats <- function() {
  
  # update currentRosters, europePlayers and College players from write_rookiesDraft.R
  current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
  rookies <- read.csv("data/rookies.csv",stringsAsFactors = FALSE)
  collegePlayers <- read.csv("data/collegePlayers.csv", stringsAsFactors = FALSE)
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  europePlayers <- read.csv("data/europePlayers.csv", stringsAsFactors = FALSE)
  playersNew <- playersHist %>%
    filter(Season == max(as.character(Season))) %>%
    mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
  
  playersNewPredicted <- data.frame()
  for (team in unique(playersNew$Tm)){
    
    thisTeam <- filter(playersNew, Tm == team)
    thisTeamStats <- data.frame()
    counter <- 0 # keep count
    for (player in thisTeam$Player){
      counter <- counter + 1
      #if (!(player %in% playersNewPredicted$Player)){ # skip running all. Start over where it failed
        thisPlayer <- filter(thisTeam, Player == player)
        #thisPlayer <- filter(playersNew, Player == player)
        print(paste0("Team: ", team,": Processing ",thisPlayer$Player, " (",round(counter*100/nrow(playersNew),1),"%)"))
        if (thisPlayer$Age < 20) { # not enough players to compare to at age 19 or younger
          thisPlayer$Age <- 20
        }
        if (thisPlayer$Age > 39) { # not enough players to compare to at age 41 or older
          thisPlayer$Age <- 39
        }
        thisPlayerStats <- .predictPlayer(thisPlayer$Player,20,thisPlayer$Age,10) %>% 
          select(Player,Pos,Season,Age,everything())
        
        if (nrow(thisPlayerStats)>0){ # in case thisPlayerStats return an empty data.frame
          if (!is.na(thisPlayerStats$effPTS)){ # rosters not yet updated so include R (last season rookies)
            #if (thisPlayer$Exp %in% c(seq(1,25,1),"R")){ # rosters not yet updated so include R (last season rookies)
            print("NBA player: OK!")
            print(thisPlayerStats)
          } else if (player %in% playersNew$Player) { # NBA player that didn't play enough minutes so I use his numbers from last season as prediction
            thisPlayerStats <- .team_preparePredict(filter(playersNew, Player == player),team)  %>%
              mutate(Age = Age + 1) %>%
              select(Player,Pos,Season,Age,everything())
        
            thisMin <- thisTeam %>% mutate(effMin = MP*G/(5*15*3936)) # Use 15 as an approximate roster size to account for effective minutes played for players with low total minutes
            teamMinutes <- sum(thisMin$effMin)
            thisMin <- #mutate(thisMin, effMin = effMin) %>%
              filter(thisMin,Player == player) %>%
              distinct(effMin) %>%
              as.numeric()
            thisPlayerStats <- mutate(thisPlayerStats, effMin = thisMin)
            
            print("NBA player: Empty predicted stats!")
            print(thisPlayerStats)
          } else { # Rookie player or returns NA stats
            # compute rookie player average stats for this player
            thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age + 1) %>%
              mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                     G = as.numeric(thisPlayer$G), GS = as.numeric(thisPlayer$GS), Tm = team) 
            thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
            print("Average player: OK!")
            print(thisPlayerStats)
            #}
          }
        } else if (player %in% playersNew$Player) { # NBA player that didn't play enough minutes so I use his numbers from last season as prediction
          thisPlayerStats <- .team_preparePredict(filter(playersNew, Player == player),team)  %>%
            mutate(Age = Age + 1) %>%
            select(Player,Pos,Season,Age,everything())
          print("NBA player: Short minutes!")
          print(thisPlayerStats)
        } else {
          thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age + 1) %>%
            mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                   G = as.numeric(thisPlayer$G), GS = as.numeric(thisPlayer$GS), Tm = team, Age) 
          thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
          print("Average player: OK!")
          print(thisPlayerStats)
        }  
        if (nrow(thisTeamStats)>0){
          thisTeamStats <- bind_rows(thisTeamStats,thisPlayerStats)
        } else{
          thisTeamStats <- thisPlayerStats
        }
      }
    if (nrow(thisTeamStats) > 0) {
      thisTeamStats <- mutate(thisTeamStats, Tm = team)
      if (nrow(playersNewPredicted)>0){
        playersNewPredicted <- bind_rows(playersNewPredicted,thisTeamStats)
      } else{
        playersNewPredicted <- thisTeamStats
      }
    }
  }
  playersNewPredicted <- distinct(playersNewPredicted, Player, Tm, .keep_all=TRUE)
  limitMinutes <- 2*quantile(playersNewPredicted$effMin,.95) # control for possible outliers
  defaultMinutes <- quantile(playersNewPredicted$effMin,.1) # assign low minutes to outliers as they most likely belong to players with very little playing time
  playersNewPredicted2 <- mutate(playersNewPredicted,effMin = ifelse(effMin > limitMinutes, defaultMinutes,effMin))
  write.csv(playersNewPredicted2, "data/playersNewPredicted_Oct20.csv", row.names = FALSE)
  
}  

.mergePredictedWithCurrent <- function(){
  
  current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
  playersNewPredicted <- read.csv("data/playersNewPredicted.csv", stringsAsFactors = FALSE)
  
  playersNewPredicted_Current <- merge(playersNewPredicted, current_rosters[,c("Player","Tm","Exp","College")], by=c("Player","Tm"), all.x=TRUE)
  
  return(playersNewPredicted_Current)
}


# Write team predicted stats new season --------
# 1. Read final team rosters from basketball reference
# 2. For each team, each player, predict their numbers for the upcoming season:
# 2.1. Non-rookies, use the predict_player functions
# 2.2. Rookies, use college or european stats.
# 2.3. Leftovers, average out from rookie players of similar age in history

# 1
.getLatestRosters <- function(thisSeason = substr(Sys.Date(),1,4)){
  
  library(httr)
  new_rosters <- data.frame()
  
  current_rosters <- data.frame()
  playersNew <- filter(playersNew,!(Tm == "TOT"))
  for (thisTeam in unique(playersNew$Tm)){
    
    url <- paste0("http://www.basketball-reference.com/teams/",thisTeam,"/",thisSeason,".html")
    if (status_code(GET(url)) == 200){ # successful response
      getRoster <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="roster"]') %>%
        html_table(fill = TRUE)
      thisRoster <- getRoster[[1]] %>% select(-`No.`)
      names(thisRoster)[which(names(thisRoster)=='')] <- "Nationality"
      thisRoster <- mutate(thisRoster, Tm = thisTeam)  
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
  
  write.csv(current_rosters, "data/currentRosters.csv",row.names = FALSE)

}
#############################################################

# 2
# current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
# # apply rename_Player_Duplicate to avoid errors when different players share same name
# # create a copy of playersHist as it already contains the modified names
# myPlayersHist <- filter(playersHist, Season == max(Season)) %>% 
#   mutate(originalPlayer = ifelse(grepl("[0-9]",substr(Player,nchar(Player),nchar(Player))),
#                                  substr(Player,1,nchar(Player)-2), Player)) %>%
#   distinct(originalPlayer,Player) %>% select(originalPlayer,PlayerNew = Player)
# 
# current_rosters <- merge(current_rosters,myPlayersHist, by.x="Player", by.y="originalPlayer",all.x=TRUE) %>%
#   #mutate(changedName = ifelse(Player == PlayerNew,0,1))
#   select(Player = PlayerNew, everything(), PlayerHist = Player)
# # RETURN NON_MATCHING PLAYERS TO EDIT THEIR NAMES MANUALLY
# nonMatching <- filter(current_rosters, is.na(Player))
# theyShouldBe <- merge(myPlayersHist,current_rosters,by.x="originalPlayer", by.y="Player",all.x=TRUE) %>%
#   filter(is.na(PlayerHist)) %>% select(Player = originalPlayer, PlayerNew)
# 
# # pattern: grepl(nonMatching) assign theyShouldBe$PlayerNew
# current_rosters[grepl("Oubre",current_rosters$PlayerHist),]$Player <- "Kelly Oubre"
# current_rosters[grepl("Nene",current_rosters$PlayerHist),]$Player <- "Nene Hilario"
# current_rosters[grepl("Gary Payton",current_rosters$PlayerHist),]$Player <- "Gary Payton 2"
# current_rosters[grepl("Glenn Robinson",current_rosters$PlayerHist),]$Player <- "Glenn Robinson 2"
# current_rosters[grepl("Taurean Prince",current_rosters$PlayerHist),]$Player <- "Taurean Waller-Prince"

.computePredictedPlayerStats <- function() {
  
  # update currentRosters, europePlayers and College players from write_rookiesDraft.R
  current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
  rookies <- read.csv("data/rookies.csv",stringsAsFactors = FALSE)
  collegePlayers <- read.csv("data/collegePlayers.csv", stringsAsFactors = FALSE)
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  europePlayers <- read.csv("data/europePlayers.csv", stringsAsFactors = FALSE)
  
  playersNewPredicted <- data.frame()
  for (team in unique(playersNew$Tm)){
  #for (team in c("CLE")){
    #thisTeam <- filter(current_rosters, Tm == team)
    thisTeam <- filter(playersNew, Tm == team)
    thisTeamStats <- data.frame()
    for (player in thisTeam$Player){
      #if (!(player %in% playersNewPredicted$Player)){ # skip running all. Start over where it failed
        thisPlayer <- filter(thisTeam, Player == player)
        print(paste0("Team: ", team,": Processing ",thisPlayer$Player))
        #if (thisPlayer$Exp %in% seq(1,25,1)){ # not a rookie
        if (thisPlayer$Age < 20) { # not enough players to compare to at age 19 or younger
          thisPlayer$Age <- 20
        }
        if (thisPlayer$Age > 39) { # not enough players to compare to at age 41 or older
          thisPlayer$Age <- 39
        }
        thisPlayerStats <- .predictPlayer(thisPlayer$Player,20,thisPlayer$Age-1,10) %>% 
          select(Player,Pos,Season,Age,everything())
        
        if (nrow(thisPlayerStats)>0){ # in case thisPlayerStats return an empty data.frame
          if (!is.na(thisPlayerStats$effPTS)){ # rosters not yet updated so include R (last season rookies)
            #if (thisPlayer$Exp %in% c(seq(1,25,1),"R")){ # rosters not yet updated so include R (last season rookies)
            print("NBA player: OK!")
            print(thisPlayerStats)
          } else { # Rookie player
            # if (nchar(thisPlayer$College)>0) { # College player
            #   thisPlayerStats <- .predictPlayerCollegeRookie(player)
            #   print("Rookie College player: OK!")
            # } else if (player %in% europePlayers$Player) { # European player
            #   thisPlayerStatsEurope <- .predictPlayerNonCollegeRookie(player)
            #   print("Rookie Europe player: OK!")
            # } else { # International player
            # compute rookie player average stats for this player
            thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age + 1) %>%
              mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                     G = as.numeric(thisPlayer$G), GS = as.numeric(thisPlayer$GS), Tm = team) 
            thisPlayerStats <- .team_preparePredict(data = thisPlayerStats, thisTeam = as.character(thisPlayer$Tm),singlePlayer = TRUE)
            #MP = as.numeric(thisPlayer$MP))
            print("Average player: OK!")
            print(thisPlayerStats)
            #}
          }
        } else {
          thisPlayerStats <- .calculate_AvgPlayer(playersNew, thisPlayer$Age + 1) %>%
            mutate(Player = as.character(thisPlayer$Player), Pos = as.character(thisPlayer$Pos), 
                   G = as.numeric(thisPlayer$G), GS = as.numeric(thisPlayer$GS), Tm = team) 
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
      if (nrow(playersNewPredicted)>0){
        playersNewPredicted <- bind_rows(playersNewPredicted,thisTeamStats)
      } else{
        playersNewPredicted <- thisTeamStats
      }
    }
    
    
  }
  playersNewPredicted <- distinct(playersNewPredicted, Player, Tm, .keep_all=TRUE)
  write.csv(playersNewPredicted, "data/playersNewPredicted.csv", row.names = FALSE)
  
}  

.mergePredictedWithCurrent <- function(){
  
  playersNewPredicted <- read.csv("data/playersNewPredicted.csv", stringsAsFactors = FALSE)
  
  playersNewPredicted_Current <- merge(playersNewPredicted, current_rosters[,c("Player","Tm","Exp","College")], by=c("Player","Tm"), all.x=TRUE)
  
  
}


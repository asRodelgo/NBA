# Write team predicted stats new season --------
# 1. Read final team rosters from basketball reference
# 2. For each team, each player, predict their numbers for the upcoming season:
# 2.1. Non-rookies, use the predict_player functions
# 2.2. Rookies, use college or european stats.
# 2.3. Leftovers, average out from rookie players of similar age in history

# 1

library(httr)
new_rosters <- data.frame()
thisSeason <- 2017

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

#############################################################

# 2
current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
# apply rename_Player_Duplicate to avoid errors when different players share same name
# create a copy of playersHist as it already contains the modified names
myPlayersHist <- filter(playersHist, Season == max(Season)) %>% 
  mutate(originalPlayer = ifelse(grepl("[0-9]",substr(Player,nchar(Player),nchar(Player))),
                                 substr(Player,1,nchar(Player)-2), Player)) %>%
  distinct(originalPlayer,Player) %>% select(originalPlayer,PlayerNew = Player)

current_rosters <- merge(current_rosters,myPlayersHist, by.x="Player", by.y="originalPlayer",all.x=TRUE) %>%
  #mutate(changedName = ifelse(Player == PlayerNew,0,1))
  select(Player = PlayerNew, everything(), PlayerHist = Player)
# RETURN NON_MATCHING PLAYERS TO EDIT THEIR NAMES MANUALLY
nonMatching <- filter(current_rosters, is.na(Player))
theyShouldBe <- merge(myPlayersHist,current_rosters,by.x="originalPlayer", by.y="Player",all.x=TRUE) %>%
  filter(is.na(PlayerHist)) %>% select(Player = originalPlayer, PlayerNew)

# pattern: grepl(nonMatching) assign theyShouldBe$PlayerNew
current_rosters[grepl("Oubre",current_rosters$PlayerHist),]$Player <- "Kelly Oubre"
current_rosters[grepl("Nene",current_rosters$PlayerHist),]$Player <- "Nene Hilario"
current_rosters[grepl("Gary Payton",current_rosters$PlayerHist),]$Player <- "Gary Payton 2"
current_rosters[grepl("Glenn Robinson",current_rosters$PlayerHist),]$Player <- "Glenn Robinson 2"
current_rosters[grepl("Taurean Prince",current_rosters$PlayerHist),]$Player <- "Taurean Waller-Prince"

#for (team in unique(current_rosters$Tm)){
for (team in c("ATL")){
  thisTeam <- filter(current_rosters, Tm == team)
  thisTeamStats <- data.frame()
  for (player in thisTeam$Player){
    thisPlayer <- filter(thisTeam, Player == player)
    if (thisPlayer$Exp %in% seq(1,25,1)){ # not a rookie
      print(paste0("Processing ",thisPlayer$Player))
      thisPlayerStats <- .predictPlayer(thisPlayer$Player,20,thisPlayer$Age-1,10) %>% 
        select(Player,Pos,Season,Age,everything())
      print(" OK!")
    } else { # Rookie player
      if (nchar(College)>0) { # College player
        thisPlayerStats <- .predictPlayerCollegeRookie(player)
      } else if (player %in% europePlayers$Player) { # European player
        thisPlayerStatsEurope <- .predictPlayerNonCollegeRookie(player)
      } else { # International player
        # compute rookie player average stats for this player
      }
    }
    if (nrow(thisTeamStats)>0){
      thisTeamStats <- bind_rows(thisTeamStats,thisPlayerStats)
    } else{
      thisTeamStats <- thisPlayerStats
    }
    
  }
}



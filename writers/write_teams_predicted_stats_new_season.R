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



# 2
current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
# apply rename_Player_Duplicate to avoid errors when different players share same name
current_rosters <- .rename_PlayerDuplicates(current_rosters)

#for (team in unique(current_rosters$Team)){
for (team in c("ATL")){
  thisTeam <- filter(current_rosters, Tm == team)
  thisTeamStats <- data.frame()
  for (player in thisTeam$Player){
    thisPlayer <- filter(thisTeam, Player == player)
    if (thisPlayer$Exp %in% seq(1,25,1)){ # not a rookie
      print(paste0("Processing ",thisPlayer$Player))
      thisPlayerStats <- .predictPlayer(thisPlayer$Player,20,thisPlayer$Age-1,10)
      print(" OK!")
    }
    if (nrow(thisTeamStats)>0){
      thisTeamStats <- bind_rows(thisTeamStats,thisPlayerStats)
    } else{
      thisTeamStats <- thisPlayerStats
    }
    
  }
}



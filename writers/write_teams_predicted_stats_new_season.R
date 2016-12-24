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
for (thisTeam in teams_list){
  
  url <- paste0("http://www.basketball-reference.com/teams/",thisTeam,"/",thisSeason,".html")
  if (status_code(GET(url)) == 200){ # successful response
    getRoster <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="roster"]') %>%
      html_table(fill = TRUE)
    thisRoster <- getRoster[[1]]
    thisRoster <- mutate(thisRoster, team = thisTeam)  
    if (nrow(current_rosters)>0){
      current_rosters <- bind_rows(current_rosters,thisRoster)
    } else{
      current_rosters <- thisRoster
    }
  }
}
names(current_rosters) <- c(names(current_rosters)[1:5],"Birth_Date","Nationality","Experience","College","Team")
# I need to compute their current ages for the prediction model is based on their age
current_rosters <- mutate(current_rosters, Age = thisSeason - as.numeric(substr(Birth_Date,nchar(Birth_Date)-3,nchar(Birth_Date))))

write.csv(current_rosters, "data/currentRosters.csv",row.names = FALSE)

# 2

for (team in unique(current_rosters$Team)){
  thisTeam <- filter(current_rosters, Team == team)
  for (player in thisTeam$Player){
    thisPlayer <- filter(thisTeam, Player == player)
    if (thisPlayer$Experience >= 1){ # not a rookie
      
    }
  }
}



# From: https://metrumresearchgroup.github.io/slickR/

library(svglite)
#library(lattice)
#library(ggplot2)
library(rvest) 
library(httr)
library(reshape2)
library(dplyr)
#library(htmlwidgets)
#library(slickR)
library(xml2)

## Download Team logos --------
nbaTeams=c("ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
           "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NO","NYK",
           "OKC","ORL","PHI","PHX","POR","SAC","SA","TOR","UTAH","WSH")

teamImg=sprintf("https://i.cdn.turner.com/nba/nba/.element/img/4.0/global/logos/512x512/bg.white/svg/%s.svg",nbaTeams)
# use my codes
nbaTeams <- gsub("BKN","BRK",nbaTeams)
nbaTeams <- gsub("CHA","CHO",nbaTeams)
nbaTeams <- gsub("PHX","PHO",nbaTeams)

for (i in 1:length(teamImg)) {
  
  download.file(teamImg[i], destfile = paste0("images/",nbaTeams[i],".svg"))
}

## Download Player pictures --------

# For players: Go to: http://www.espn.com/nba/depth/_/type/full to extract players page
# Read player by player. Ex: http://www.espn.com/nba/player/_/id/3032979/dennis-schroder
# Access image XPath: '//*[@id="content"]/div[3]/div[2]/div[2]/img'

playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv", stringsAsFactors = FALSE)
players_pics <- data.frame()

for (t in nbaTeams) {
  url <- paste0("http://www.espn.com/nba/team/roster/_/name/",tolower(t))
  if (status_code(GET(url))==200) {
    thisTeam <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="my-players-table"]/div[2]/div/table[1]') %>%
      html_children() %>%
      html_children() %>%
      html_children() %>%
      html_attr('href')
  
    thisRoster <- thisTeam[6:length(thisTeam)]
    thisRoster <- gsub("http://www.espn.com/nba/player/_/id/","",thisRoster)
    thisPair <- strsplit(thisRoster,"/",fixed = TRUE) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    names(thisPair) <- c("player_code","player_name")
    
    if (nrow(players_pics)>0){
      players_pics <- rbind(players_pics,thisPair)
    } else {
      players_pics <- thisPair
    }
  }  
}
write.csv(players_pics, "data/players_pics_codes.csv", row.names = FALSE)

for (p in 103:nrow(players_pics)) {
  
  thisPic <- paste0("http://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/",
                    players_pics[p,1],".png&w=350&h=254")
  if (status_code(GET(thisPic))==200) {
    download.file(thisPic,destfile = paste0("images/",players_pics[p,2],".png"))
  }
  
} 


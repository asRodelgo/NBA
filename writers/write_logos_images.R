# From: https://metrumresearchgroup.github.io/slickR/

library(svglite)
library(lattice)
library(ggplot2)
library(rvest) 
library(httr)
library(reshape2)
library(dplyr)
library(htmlwidgets)
library(slickR)
library(xml2)

## Download Team logos --------
nbaTeams=c("ATL","BOS","BKN","CHA","CHI","CLE","DAL","DEN","DET","GSW",
           "HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK",
           "OKC","ORL","PHI","PHX","POR","SAC","SAS","TOR","UTA","WAS")

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
players_pics <- tolower(gsub(" ","-",playersPredictedStats_adjPer$Player))

for (p in 1:length(players_pics)) {
  url <- paste0("http://www.espn.com/nba/player/_/id/3032979/",players_pics[p])
  if (status_code(GET(url))==200) {
    thisPic <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="content"]/div[3]/div[2]/div[2]/img') %>%
      html_attr('src')
    download.file(thisPic,destfile = paste0("images/",players_pics[p],".svg"))
  }
  
}

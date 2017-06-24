# Write team stats by season ---------------------------

require(httr)
require(tidyverse)
library(rvest)
thisYear <- substr(Sys.Date(),1,4)
playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE)

##### ALL SEASONS ########
firstYear <- 1979

##### NEW SEASON ########
firstYear <- thisYear

# Read teams stats for all seasons
teamStats <- data.frame()
lastSeason <- as.numeric(substr(max(as.character(playersHist$Season)),1,4))
for (year in firstYear:thisYear){
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_",year,".html")
  # Eastern conference
  thisSeasonStats <- url %>%
    read_html() %>%
    #html_nodes(xpath='//*[@id="all_standings"]/table') %>%
    html_nodes(xpath='//*[@id="confs_standings_E"]') %>%
    html_table(fill = TRUE)
  thisSeasonStats_E <- thisSeasonStats[[1]]
  names(thisSeasonStats_E)[1] <- "Team"
  # Western conference
  thisSeasonStats <- url %>%
    read_html() %>%
    #html_nodes(xpath='//*[@id="all_standings"]/table') %>%
    html_nodes(xpath='//*[@id="confs_standings_W"]') %>%
    html_table(fill = TRUE)
  thisSeasonStats_W <- thisSeasonStats[[1]]
  names(thisSeasonStats_W)[1] <- "Team"
  # All together
  thisSeasonStats <- bind_rows(thisSeasonStats_E,thisSeasonStats_W)
  #thisSeasonStats <- thisSeasonStats[2:nrow(thisSeasonStats),1:8]
  #names(thisSeasonStats) <- c("Team",thisSeasonStats[1,2:ncol(thisSeasonStats)])
  #thisSeasonStats <- thisSeasonStats[-1,]
  #thisSeasonStats <- thisSeasonStats[!(thisSeasonStats$W=="W"),]
  #thisSeasonStats <- thisSeasonStats[!grepl("division",tolower(thisSeasonStats[,1])),]
  thisSeasonStats <- select(thisSeasonStats, -`W/L%`, -GB)
  thisSeasonStats <- mutate_each(thisSeasonStats, funs(as.numeric), -Team)
  thisSeasonStats$Team <- gsub("\\*?\\([0-9]+\\)","",thisSeasonStats$Team)
  thisSeasonStats$Team <- gsub("*","",trim,fixed = TRUE)
  thisSeasonStats$Team <- trimws(thisSeasonStats$Team)
  names(thisSeasonStats) <- gsub("PS/G","PTS",names(thisSeasonStats))
  names(thisSeasonStats) <- gsub("PA/G","PTSA",names(thisSeasonStats))
  # trim <- gsub("\\*?\\([0-9]+\\)","",thisSeasonStats$Team)
  # trim <- gsub("*","",trim,fixed = TRUE)
  # blank <- substr(trim[1],nchar(trim[1]),nchar(trim[1]))
  # trim <- gsub(blank,"",trim)
  # thisSeasonStats$Team <- trim
  thisSeasonStats$Season <- paste0(year-1,"-",year)
  if (nrow(teamStats)>0) {
    teamStats <- bind_rows(teamStats,thisSeasonStats)
  } else {
    teamStats <- thisSeasonStats
  }
}

if (firstYear==thisYear) {
  teamStatsOLD <- read.csv("data/teamStats.csv", stringsAsFactors = FALSE)
  teamStats <- bind_rows(teamStatsOLD,teamStats)
} 


write.csv(teamStats, "data/teamStats.csv",row.names = FALSE)



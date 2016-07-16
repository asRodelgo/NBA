# Write team stats by season ---------------------------

# Read teams stats for all seasons
team_stats <- data.frame()
lastSeason <- as.numeric(substr(max(as.character(playersHist$Season)),1,4))
for (year in 1979:lastSeason){
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_",year+1,".html")
  thisSeasonStats <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="all_standings"]/table') %>%
    html_table(fill = TRUE)
  thisSeasonStats <- thisSeasonStats[[1]]
  thisSeasonStats <- thisSeasonStats[2:nrow(thisSeasonStats),1:8]
  names(thisSeasonStats) <- c("Team",thisSeasonStats[1,2:ncol(thisSeasonStats)])
  thisSeasonStats <- thisSeasonStats[-1,]
  thisSeasonStats <- thisSeasonStats[!(thisSeasonStats$W=="W"),]
  thisSeasonStats <- thisSeasonStats[!grepl("division",tolower(thisSeasonStats[,1])),]
  thisSeasonStats <- select(thisSeasonStats, -`W/L%`, -GB)
  thisSeasonStats <- mutate_each(thisSeasonStats, funs(as.numeric), -Team)
  trim <- gsub("\\*?\\([0-9]+\\)","",thisSeasonStats$Team)
  trim <- gsub("*","",trim,fixed = TRUE)
  blank <- substr(trim[1],nchar(trim[1]),nchar(trim[1]))
  trim <- gsub(blank,"",trim)
  thisSeasonStats$Team <- trim
  thisSeasonStats$Season <- paste0(year,"-",year+1)
  if (nrow(team_stats)>0) {
    team_stats <- bind_rows(team_stats,thisSeasonStats)
  } else {
    team_stats <- thisSeasonStats
  }
}
write.csv(team_stats, "data/teamStats.csv",row.names = FALSE)



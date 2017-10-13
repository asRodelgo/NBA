# Estimate standard deviation of points scored accross seasons

write_gameScores <- function() {
  
  require(httr)
  require(tidyverse)
  library(rvest)
  thisYear <- substr(Sys.Date(),1,4)
  playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE)
  
  ##### ALL SEASONS ########
  firstYear <- 2000
  
  # Read teams stats for all seasons
  teamPoints <- data.frame()
  
  for (year in firstYear:thisYear){
    for (month in c("october","november","december","january","february","march","april","may","june")) {
      
      url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-",month,".html")
      if (status_code(GET(url)) == 200) {
        
        thisMonthStats <- url %>%
          read_html() %>%
          #html_nodes(xpath='//*[@id="all_standings"]/table') %>%
          html_nodes(xpath='//*[@id="schedule"]') %>%
          html_table(fill = TRUE)
        thisMonthStats <- thisMonthStats[[1]]
        thisMonthStats <- thisMonthStats[,c(1,4,6)]
        names(thisMonthStats) <- c("Date","pts_away","pts_home")
        
        if (nrow(teamPoints) > 0) {
          teamPoints <- rbind(teamPoints,thisMonthStats)
        } else {
          teamPoints <- thisMonthStats
        }
        
      }
      
    }
  }
  
  write.csv(teamPoints, "data/gameScores.csv", row.names = FALSE)
}
  
  
# Estimate score differential factoring home or away



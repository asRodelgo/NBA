## Update the historical players database: playersHist

# Look in basketballreference.com and loop for all seasons
# Example: http://www.basketball-reference.com/leagues/NBA_2017_per_game.html

require(httr)
require(tidyverse)
library(rvest)
thisYear <- substr(Sys.Date(),1,4)

##### ALL SEASONS ########
firstYear <- 1980

##### NEW SEASON ########
firstYear <- thisYear

playersHist <- data.frame()
for (year in firstYear:thisYear){
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_",year,"_per_game.html")
  thisSeasonStats <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="per_game_stats"]') %>%
    html_table(fill = TRUE)
  thisSeasonStats <- thisSeasonStats[[1]] %>% filter(!(Player == "Player")) %>% 
    mutate(Season = paste0(year-1,"-",year)) %>%
    select(-Rk)
  
  if (nrow(playersHist)>0) playersHist <- bind_rows(playersHist,thisSeasonStats) 
  else playersHist <- thisSeasonStats
}

playersHist <- mutate_at(playersHist, vars(c(3,5:(ncol(playersHist)-1))), funs(as.numeric))
playersHist[is.na(playersHist)] <- 0
names(playersHist) <- gsub("%",".",names(playersHist),fixed=TRUE)
names(playersHist) <- gsub("2","X2",names(playersHist))
names(playersHist) <- gsub("3","X3",names(playersHist))
names(playersHist) <- gsub("PS/G","PTS",names(playersHist),fixed=TRUE)

if (firstYear==thisYear) {
  playersHistOLD <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE)
  playersHist <- bind_rows(playersHistOLD,playersHist)
} 
playersHist<- filter(playersHist, Season > "1978-1979")
write.csv(playersHist, "data/playersHist.csv",row.names = FALSE)  
  
  
# Playoff scores --------
# compute playoff series between 2 teams (best of 7)
.computeSeries <- function(teamA,teamB){
  
  # teamA has home court advantage
  series <- data.frame()
  winA <- winB <- 0
  while (max(winA,winB)<4){ # best of 7
    if ((winA + winB) %in% c(0,1,4,6)){ # teamA home games
      thisGame <- .calculateScore(teamA,teamB)
      if (thisGame[1]>thisGame[2]){
        winA <- winA + 1
      } else {
        winB <- winB + 1
      }
    } else { # teamB home games
      thisGame <- .calculateScore(teamB,teamA)
      if (thisGame[2]>thisGame[1]){
        winA <- winA + 1
      } else {
        winB <- winB + 1
      }
    }
    series[winA+winB,1] <- thisGame[1]
    series[winA+winB,2] <- thisGame[2]
    series[winA+winB,3] <- thisGame[3]
    series[winA+winB,4] <- winA
  }
  
  return(series)
}

# compute playoffs. Input is top 8 team codes from both conferences
.computePlayoffs <- function(pEast,pWest) {
  
  seriesEast <- data.frame(teamCode=pEast, conference = "E", round=8, stringsAsFactors = FALSE)
  seriesWest <- data.frame(teamCode=pWest, conference = "W", round=8, stringsAsFactors = FALSE)
  playoffSeries <- bind_rows(seriesEast,seriesWest)
  
  for (conf in c("E","W")){
    offset <- ifelse(conf=="E",0,8) # store East and West in data frame
    # Conf Quarters
    for (i in 1:4){
      thisSeries <- .computeSeries(playoffSeries[i+offset,1],playoffSeries[8-i+offset+1,1])
      if (thisSeries[nrow(thisSeries),4]==4){
        playoffSeries[16+i+offset,1] <- playoffSeries[i+offset,1]
      } else {
        playoffSeries[16+i+offset,1] <- playoffSeries[8-i+offset+1,1]
      }
      playoffSeries$round[16+i+offset] <- 4
      playoffSeries$conference[16+i+offset] <- conf
    }
    # Conf Semis
    for (i in 1:2){
      thisSeries <- .computeSeries(playoffSeries[16+i+offset,1],playoffSeries[20-i+offset+1,1])
      if (thisSeries[nrow(thisSeries),4]==4){
        playoffSeries[20+i+offset,1] <- playoffSeries[16+i+offset,1]
      } else {
        playoffSeries[20+i+offset,1] <- playoffSeries[20-i+offset+1,1]
      }
      playoffSeries$round[20+i+offset] <- 2
      playoffSeries$conference[20+i+offset] <- conf
    }
    # Conf Finals
    thisSeries <- .computeSeries(playoffSeries[21+offset,1],playoffSeries[22+offset,1])
    if (thisSeries[nrow(thisSeries),4]==4){
      playoffSeries[23+offset,1] <- playoffSeries[21+offset,1]
    } else {
      playoffSeries[23+offset,1] <- playoffSeries[22+offset,1]
    }
    playoffSeries$round[23+offset] <- 1
    playoffSeries$conference[23+offset] <- conf
  }
  playoffSeries <- filter(playoffSeries, !is.na(teamCode))
  
  # Finals
  finals <- .computeSeries(filter(playoffSeries,conference == "E", round == 1)$teamCode,
                           filter(playoffSeries,conference == "W", round == 1)$teamCode)
  playoffSeries[nrow(playoffSeries)+1,1] <- ifelse(finals[nrow(thisSeries),4]==4,
                                                    playoffSeries[23,1], playoffSeries[30,1])
  playoffSeries$round[nrow(playoffSeries)] <- 0
  playoffSeries$conference[nrow(playoffSeries)] <- conferences[conferences$TeamCode == playoffSeries[nrow(playoffSeries),1],3]
  
  return(playoffSeries)
}





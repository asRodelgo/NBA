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

# Plot playoff bracket
.getPlayoffResults <- function(){

  standings <- regSeasonOutcome[[1]]
  
  pEast <- as.character(head(arrange(filter(dplyr::select(standings[[tail(datesRange,1)]], teamCode,conference,win,lose), conference == "E"), desc(win/(win+lose))),8)$teamCode)
  pWest <- as.character(head(arrange(filter(dplyr::select(standings[[tail(datesRange,1)]], teamCode,conference,win,lose), conference == "W"), desc(win/(win+lose))),8)$teamCode)
  
  playoffs <- .computePlayoffs(pEast,pWest)
  return(playoffs)
  
}

.playoffBracket <- function(round){
  
  # Print bracket -----------------
  x<-seq(0,220,(221/67))
  y<-0:66
  
  plot(x,y,type="l", col.axis="white", col.lab="white", bty="n", 
       axes=F, col="white")
  
  # left hand side bracket
  segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
  segments(60,c(3,19,37,53),60,c(11,27,45,61))
  segments(60,c(7,23,41,57),80,c(7,23,41,57))
  segments(80,c(7,41),80,c(23,57))
  segments(80,c(15,49),100,c(15,49))
  # central part
  segments(100,c(27,37),120,c(27,37))
  # right hand side bracket
  segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
  segments(160,c(3,19,37,53),160,c(11,27,45,61))
  segments(140,c(7,23,41,57),160,c(7,23,41,57))
  segments(140,c(7,41),140,c(23,57))
  segments(120,c(15,49),140,c(15,49))
  
  # team names ----------------------
  # West quarterfinals
  text(49.8,61.5,filter(playoffs,conference == "W",round==8)$teamCode[1],cex=1)
  text(49.8,53.5,filter(playoffs,conference == "W",round==8)$teamCode[8],cex=1)
  text(49.8,45.5,filter(playoffs,conference == "W",round==8)$teamCode[4],cex=1)
  text(49.8,37.5,filter(playoffs,conference == "W",round==8)$teamCode[5],cex=1)
  
  text(49.8,27.5,filter(playoffs,conference == "W",round==8)$teamCode[2],cex=1)
  text(49.8,19.5,filter(playoffs,conference == "W",round==8)$teamCode[7],cex=1)
  text(49.8,11.5,filter(playoffs,conference == "W",round==8)$teamCode[3],cex=1)
  text(49.8,3.5,filter(playoffs,conference == "W",round==8)$teamCode[6],cex=1)
  
  # East quarterfinals
  text(169.8,61.5,filter(playoffs,conference == "E",round==8)$teamCode[1],cex=1)
  text(169.8,53.5,filter(playoffs,conference == "E",round==8)$teamCode[8],cex=1)
  text(169.8,45.5,filter(playoffs,conference == "E",round==8)$teamCode[4],cex=1)
  text(169.8,37.5,filter(playoffs,conference == "E",round==8)$teamCode[5],cex=1)
  
  text(169.8,27.5,filter(playoffs,conference == "E",round==8)$teamCode[2],cex=1)
  text(169.8,19.5,filter(playoffs,conference == "E",round==8)$teamCode[7],cex=1)
  text(169.8,11.5,filter(playoffs,conference == "E",round==8)$teamCode[3],cex=1)
  text(169.8,3.5,filter(playoffs,conference == "E",round==8)$teamCode[6],cex=1)
  
  rounds <- c(8)
  if (round == "Conference Semifinals"){
    rounds <- c(8,4)
  } else if (round == "Conference Finals") {
    rounds <- c(8,4,2)
  } else if (round == "Finals"){
    rounds <- c(8,4,2,1)
  } else if (round == "Champion"){
    rounds <- c(8,4,2,1,0)
  }
  
  if (4 %in% rounds){
    # West semifinals
    text(69.8,57.5,filter(playoffs,conference == "W",round==4)$teamCode[1],cex=1)
    text(69.8,41.5,filter(playoffs,conference == "W",round==4)$teamCode[4],cex=1)
    text(69.8,23.5,filter(playoffs,conference == "W",round==4)$teamCode[2],cex=1)
    text(69.8,7.5,filter(playoffs,conference == "W",round==4)$teamCode[3],cex=1)
    # East semifinals
    text(149.8,57.5,filter(playoffs,conference == "E",round==4)$teamCode[1],cex=1)
    text(149.8,41.5,filter(playoffs,conference == "E",round==4)$teamCode[4],cex=1)
    text(149.8,23.5,filter(playoffs,conference == "E",round==4)$teamCode[2],cex=1)
    text(149.8,7.5,filter(playoffs,conference == "E",round==4)$teamCode[3],cex=1)
    
  }
  
  if (2 %in% rounds){
    # Conference Finals
    text(89.8,49.5,filter(playoffs,conference == "W",round==2)$teamCode[1],cex=1)
    text(129.8,49.5,filter(playoffs,conference == "E",round==2)$teamCode[1],cex=1)
    text(89.8,15.5,filter(playoffs,conference == "W",round==2)$teamCode[2],cex=1)
    text(129.8,15.5,filter(playoffs,conference == "E",round==2)$teamCode[2],cex=1)
  }
  
  if (1 %in% rounds){
    # Finals
    text(109.8,37.5,filter(playoffs,conference == "W",round==1)$teamCode[1],cex=1)
    text(109.8,27.5,filter(playoffs,conference == "E",round==1)$teamCode[1],cex=1)
  } 
  
  if (0 %in% rounds){
    # Champion
    text(109.8,32.5,filter(playoffs,round==0)$teamCode[1],cex=2)
  }
}



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
  }
  
  return(series)
}

series <- .computeSeries("SAS","HOU")



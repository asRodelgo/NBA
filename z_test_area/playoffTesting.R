# Testing regular season ----------------------------------
regSeasonOutcome <- .standings(real=TRUE)
standings <- regSeasonOutcome[[1]]
scores <- regSeasonOutcome[[2]]

x <- 370

pEast <- as.character(head(arrange(filter(dplyr::select(standings[[tail(datesRange,1)]], teamCode,conference,win,lose), conference == "E"), desc(win/(win+lose))),8)$teamCode)
pWest <- as.character(head(arrange(filter(dplyr::select(standings[[tail(datesRange,1)]], teamCode,conference,win,lose), conference == "W"), desc(win/(win+lose))),8)$teamCode)

playoffs <- .computePlayoffs(pEast,pWest)

series <- .computeSeries("SAS","HOU")

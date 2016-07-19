# Testing regular season ----------------------------------
regSeasonOutcome <- .standings()
standings <- regSeasonOutcome[[1]]
scores <- regSeasonOutcome[[2]]

x <- 370
teamW <- "GSW"
teamE <- "CLE"
arrange(filter(standings[[x]], conference == "E"), desc(win/(win+lose)))
arrange(filter(standings[[x]], conference == "W"), desc(win/(win+lose)))

filter(scores, (day <= x) & (home_team == teamW | away_team == teamW))
filter(scores, (day <= x) & (home_team == teamE | away_team == teamE))
filter(scores, (home_team == team | away_team == team))

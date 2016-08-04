# Testing regular season ----------------------------------
regSeasonOutcome <- .standings()
standings <- regSeasonOutcome[[1]]
scores <- regSeasonOutcome[[2]]

x <- 370
teamW <- "GSW"
teamE <- "CLE"
arrange(filter(dplyr::select(standings[[x]], team,conference,win,lose,win_home_perc,win_conf_perc,
                      avg_pts,avg_pts_ag,streak), conference == "E"), desc(win/(win+lose)))
arrange(filter(dplyr::select(standings[[x]], team,conference,win,lose,win_home_perc,win_conf_perc,
                             avg_pts,avg_pts_ag,streak), conference == "W"), desc(win/(win+lose)))

arrange(filter(standings[[x]]), desc(win/(win+lose)))

filter(scores, (day <= x) & (home_team == teamW | away_team == teamW))
filter(scores, (day <= x) & (home_team == teamE | away_team == teamE))
filter(scores, (home_team == teamW | away_team == teamW))

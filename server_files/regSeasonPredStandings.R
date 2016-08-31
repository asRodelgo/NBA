# regular season predicted standings

# West
output$westPredStandings <- DT::renderDataTable({
  regSeasonOutcome <- .standings()
  standings <- regSeasonOutcome[[1]]
  total_days <- length(standings)
  westPredStandings <- arrange(filter(dplyr::select(standings[[total_days]], team,conference,win,lose,win_home_perc,win_conf_perc,
                                                    avg_pts,avg_pts_ag,streak), conference == "W"), desc(win/(win+lose)))
  return(westPredStandings)
},options = list(dom = 't'))

# East
output$westPredStandings <- DT::renderDataTable({
  regSeasonOutcome <- .standings()
  standings <- regSeasonOutcome[[1]]
  total_days <- length(standings)
  westPredStandings <- arrange(filter(dplyr::select(standings[[total_days]], team,conference,win,lose,win_home_perc,win_conf_perc,
                                                    avg_pts,avg_pts_ag,streak), conference == "E"), desc(win/(win+lose)))
  return(westPredStandings)
},options = list(dom = 't'))

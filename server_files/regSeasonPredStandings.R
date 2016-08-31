# regular season predicted standings

# West
output$westPredStandings <- DT::renderDataTable({
  westPredStandings <- .getConferenceStandings("W")
  return(westPredStandings)
},options = list(dom = 't', pageLength = 16))

# East
output$eastPredStandings <- DT::renderDataTable({
  eastPredStandings <- .getConferenceStandings("E")
  return(eastPredStandings)
},options = list(dom = 't', pageLength = 16))

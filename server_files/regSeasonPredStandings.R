# regular season predicted standings

# West
output$westPredStandings <- DT::renderDataTable({
  westPredStandings <- .getConferenceStandings("W",input$regSeasonDay)
  return(westPredStandings)
},options = list(dom = 't', pageLength = 16))

# East
output$eastPredStandings <- DT::renderDataTable({
  eastPredStandings <- .getConferenceStandings("E",input$regSeasonDay)
  return(eastPredStandings)
},options = list(dom = 't', pageLength = 16))

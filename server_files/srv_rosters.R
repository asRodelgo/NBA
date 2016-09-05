# Get rosters-------

output$teamRoster <- DT::renderDataTable({
  roster <- filter(playersNew, Tm == input$inTeam)
  return(roster)
},options = list(dom = 't', pageLength = 25))

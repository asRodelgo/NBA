# reg season games

output$PredGames <- DT::renderDataTable({
  predGames <- .getGames("W",input$regSeasonDay)
  return(predGames)
},options = list(dom = 't', pageLength = 16))

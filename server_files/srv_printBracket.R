# Print a playoff bracket ---------------------

output$playoffBracket <- renderPlot({
  bracket <- .playoffBracket(input$inPlayoffRound)
  return(bracket)
})

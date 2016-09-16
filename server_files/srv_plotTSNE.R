# Plot tSNE ---------------------

output$plotTSNE <- renderPlot({
  plotTSNE <- .tSNE_plot_All(input$colTeam,input$colSeason,input$colPlayer,
                             input$colAge,input$colSkill)
  return(plotTSNE)
})

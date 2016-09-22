# Plot tSNE ---------------------

output$plotTSNE <- renderPlot({
  plotTSNE <- .tSNE_plot_All(input$colTeam,input$colSeason,input$colPlayer,
                             input$colAge,input$colSkill)
  return(plotTSNE)
})

# tooltip hover over scatterplot points: see https://gitlab.com/snippets/16220
output$hover_info <- renderUI({
  hover <- input$plot_hover
  point <- nearPoints(.tSNE_plot_filter(input$colTeam,input$colSeason,input$colPlayer,
                                     input$colAge,input$colSkill), hover, threshold = 3, maxpoints = 1, addDist = TRUE)
  
  if (nrow(point) == 0) return(NULL)
  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  
  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
  
  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(paste0(point$Player, "<br/>",point$Tm," - ",point$Season, "<b> (Age: </b>", point$Age, ") <br/>",
                  input$colSkill,": ",eval(parse(text=paste0("point$",input$colSkill))))))
  )
})
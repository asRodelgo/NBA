# regSeasonPredGames

fluidPage(
  column(6,
         h4("Fixtures", style="color:#3399ff"),   
         DT::dataTableOutput('PredGames'),br()
  )
)
  
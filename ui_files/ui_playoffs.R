# Playoffs UI ----------

fluidPage(
  column(12,
         selectInput('inPlayoffRound', 'Playoff Round:', choices=c("Select round","Conference Semifinals",
                                                                   "Conference Finals","Finals","Champion"),selectize=FALSE), 
         br(),
         plotOutput('playoffBracket'),br()
  )
)




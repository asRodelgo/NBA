fluidPage(
  column(12,
         selectInput('inTeam', 'Select a team:', choices=team_statsNew$teamCode,selectize=FALSE), 
         br(),
         DT::dataTableOutput('teamRoster'),br()
  )
)

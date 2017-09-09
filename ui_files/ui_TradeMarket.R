fluidPage(
  column(3,
         selectInput('tradeTeamA', 'Team A', choices=team_statsNew$teamCode,selectize=FALSE), 
         br(),
         DT::dataTableOutput('teamPowerA'),
         DT::dataTableOutput('teamPowerA_new'),
         br(),
         checkboxGroupInput('tradePlayersA', label = "Roster. Check players to trade", 
                            choices = filter(playersNew, Tm == team_statsNew$teamCode[1])$Player, 
                            selected = NULL)
         
  ),
  column(2, actionButton("tradeButton", label = "Make this trade!")),
  column(3,
         selectInput('tradeTeamB', 'Team B', choices=team_statsNew$teamCode,selectize=FALSE), 
         br(),
         DT::dataTableOutput('teamPowerB'),
         DT::dataTableOutput('teamPowerB_new'),
         br(),
         checkboxGroupInput('tradePlayersB', label = "Roster. Check players to trade", 
                            choices = filter(playersNew, Tm == team_statsNew$teamCode[1])$Player, 
                            selected = NULL)
  ),
  column(4, DT::dataTableOutput('teamPower_all'))
)



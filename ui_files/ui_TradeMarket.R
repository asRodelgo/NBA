fluidPage(
  column(5,
         selectInput('tradeTeamA', 'Team A', choices=team_statsNew$teamCode,selectize=FALSE), 
         br(),
         DT::dataTableOutput('teamPowerA'),br(),
         checkboxGroupInput('tradePlayersA', "Roster. Check players to trade", 
                            choices = filter(playersNew, Tm == input$tradeTeamA) %>%
                              select(Player, MP, G) %>%
                              mutate(Percent_Played = round((MP*G*100)/(82*48),2),
                                     Player_Label = paste0(Player," (% Minutes: ",Percent_Played)) %>%
                              arrange(desc(Percent_Played)) %>%
                              select(Player_Label), 
                            selected = NULL)
         
  ),
  column(2, actionButton("tradeButton")),
  column(5,
         selectInput('tradeTeamB', 'Team B', choices=team_statsNew$teamCode,selectize=FALSE), 
         br(),
         DT::dataTableOutput('teamPowerB'),br(),
         checkboxGroupInput('tradePlayersB', "Roster. Check players to trade", 
                            choices = filter(playersNew, Tm == input$tradeTeamB) %>%
                              select(Player, MP, G) %>%
                              mutate(Percent_Played = round((MP*G*100)/(82*48),2),
                                     Player_Label = paste0(Player," (% Minutes: ",Percent_Played)) %>%
                              arrange(desc(Percent_Played)) %>%
                              select(Player_Label), 
                            selected = NULL)
  )
)
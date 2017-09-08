fluidPage(
  column(5,
         selectInput('tradeTeamA', 'Team A', choices=team_statsNew$teamCode,selectize=FALSE), 
         br(),
         DT::dataTableOutput('teamPowerA'),
         DT::dataTableOutput('teamPowerA_new'),
         br(),
         checkboxGroupInput('tradePlayersA', label = "Roster. Check players to trade", 
                            choices = c(filter(playersNew, Tm == team_statsNew$teamCode[1]) %>%
                                          select(Player, MP, G) %>%
                                          mutate(Percent_Played = round((MP*G*100)/(82*48),2),
                                                 Player_Label = paste0(Player," (% Minutes: ",Percent_Played,")")) %>%
                                          arrange(desc(Percent_Played)) %>%
                                          select(Player_Label))$Player_Label, 
                            selected = NULL)
         
  ),
  column(2, actionButton("tradeButton", label = "Make this trade!")),
  column(5,
         selectInput('tradeTeamB', 'Team B', choices=team_statsNew$teamCode,selectize=FALSE), 
         br(),
         DT::dataTableOutput('teamPowerB'),
         DT::dataTableOutput('teamPowerB_new'),
         br(),
         checkboxGroupInput('tradePlayersB', label = "Roster. Check players to trade", 
                            choices = c(filter(playersNew, Tm == team_statsNew$teamCode[1]) %>%
                              select(Player, MP, G) %>%
                              mutate(Percent_Played = round((MP*G*100)/(82*48),2),
                                     Player_Label = paste0(Player," (% Minutes: ",Percent_Played,")")) %>%
                              arrange(desc(Percent_Played)) %>%
                              select(Player_Label))$Player_Label, 
                            selected = NULL)
  )
)



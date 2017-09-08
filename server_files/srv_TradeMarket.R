# Get rosters-------

output$teamPowerA <- DT::renderDataTable({
  
  power <- filter(teamsPredicted, TeamCode == input$tradeTeamA) %>% 
    select(Offense = TEAM_PTS, Defense = TEAM_PTSAG)
  return(power)
},options = list(dom = 't'))#, rownames = FALSE)

output$teamPowerB <- DT::renderDataTable({
  
  power <- filter(teamsPredicted, TeamCode == input$tradeTeamB) %>% 
    select(Offense = TEAM_PTS, Defense = TEAM_PTSAG)
  return(power)
},options = list(dom = 't'))#, rownames = FALSE)

observe({
  
  selectedTeamA <- input$tradeTeamA
  selectedTeamB <- input$tradeTeamB
  #if (!exists(newData)) newData <- playersNew
  
  updateCheckboxGroupInput(session, 'tradePlayersA', 
                           choices = c(filter(newData, Tm == selectedTeamA) %>%
                             select(Player, MP, G) %>%
                             mutate(Percent_Played = round((MP*G*100)/(82*48),2),
                                    Player_Label = paste0(Player," (% Minutes: ",Percent_Played)) %>%
                             arrange(desc(Percent_Played)) %>%
                             select(Player_Label))$Player_Label, 
                           selected = NULL)
  
  updateCheckboxGroupInput(session, 'tradePlayersB', 
                           choices = c(filter(newData, Tm == selectedTeamB) %>%
                                         select(Player, MP, G) %>%
                                         mutate(Percent_Played = round((MP*G*100)/(82*48),2),
                                                Player_Label = paste0(Player," (% Minutes: ",Percent_Played)) %>%
                                         arrange(desc(Percent_Played)) %>%
                                         select(Player_Label))$Player_Label, 
                           selected = NULL)
  
})

observeEvent(input$tradeButton,{
  
  # update teamPowerA and B by calculating new powers
  newData <- .trade_Players(playersNew, input$tradePlayersA,input$tradeTeamA,input$tradePlayersB,input$tradeTeamB) 
  # update checkboxGroupInput
  updateCheckboxGroupInput(session, 'tradePlayersA', 
                           choices = c(filter(newData, Tm == input$tradeTeamA) %>%
                             select(Player, MP, G) %>%
                             mutate(Percent_Played = round((MP*G*100)/(82*48),2),
                                    Player_Label = paste0(Player," (% Minutes: ",Percent_Played)) %>%
                             arrange(desc(Percent_Played)) %>%
                             select(Player_Label))$Player_Label, 
                           selected = NULL)
  updateCheckboxGroupInput(session, 'tradePlayersB', 
                           choices = c(filter(newData, Tm == input$tradeTeamB) %>%
                             select(Player, MP, G) %>%
                             mutate(Percent_Played = round((MP*G*100)/(82*48),2),
                                    Player_Label = paste0(Player," (% Minutes: ",Percent_Played)) %>%
                             arrange(desc(Percent_Played)) %>%
                             select(Player_Label))$Player_Label, 
                           selected = NULL)
  
})

# Take a reactive dependency on input$button, but
# not on any of the stuff inside the function
powerA_new <- eventReactive(input$tradeButton, {
  #if (!exists(newData)) newData <- playersNew
  newPower <- merge(.computePower(newData,"PTS",input$tradeTeamA),.computePower(newData,"PTSA",input$tradeTeamA),by="team_season") %>% 
    as.data.frame()
  newPower <- newPower[,-1]
  names(newPower) <- c("Offense","Defense")  
  return(newPower)
})
output$teamPowerA_new <- renderDataTable({
  powerA_new()
},options = list(dom = 't'))#, rownames = FALSE)

powerB_new <- eventReactive(input$tradeButton, {
  #if (!exists(newData)) newData <- playersNew
  newPower <- merge(.computePower(newData,"PTS",input$tradeTeamB),.computePower(newData,"PTSA",input$tradeTeamB),by="team_season") %>% 
    as.data.frame()
  newPower <- newPower[,-1]
  names(newPower) <- c("Offense","Defense")  
  return(newPower)
})
output$teamPowerB_new <- renderDataTable({
  powerB_new()
},options = list(dom = 't'))#, rownames = FALSE)


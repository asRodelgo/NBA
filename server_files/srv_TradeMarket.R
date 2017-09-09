# Get rosters-------

output$teamPowerA <- DT::renderDataTable({
  
  power <- filter(teamsPredicted, TeamCode == input$tradeTeamA) %>% 
    select(Offense = TEAM_PTS, Defense = TEAM_PTSAG) %>%
    mutate(Offense = round(Offense,2), Defense = round(Defense,2))
  return(power)
},options = list(dom = 't'))#, rownames = FALSE)

output$teamPowerB <- DT::renderDataTable({
  
  power <- filter(teamsPredicted, TeamCode == input$tradeTeamB) %>% 
    select(Offense = TEAM_PTS, Defense = TEAM_PTSAG) %>%
    mutate(Offense = round(Offense,2), Defense = round(Defense,2))
  return(power)
},options = list(dom = 't'))#, rownames = FALSE)

observe({
  
  selectedTeamA <- input$tradeTeamA
  selectedTeamB <- input$tradeTeamB
  #if (!exists(newData)) newData <- playersNew
  
  # choices = c(filter(newData, Tm == selectedTeamA) %>%
  #               select(Player, MP, G) %>%
  #               mutate(Percent_Played = round((MP*G*100)/(82*48),2),
  #                      Player_Label = paste0(Player," (% Minutes: ",Percent_Played)) %>%
  #               arrange(desc(Percent_Played)) %>%
  #               select(Player_Label))$Player_Label, 
  
  updateCheckboxGroupInput(session, 'tradePlayersA', 
                           choices = filter(newData, Tm == selectedTeamA)$Player, 
                           selected = NULL)
  
  updateCheckboxGroupInput(session, 'tradePlayersB', 
                           choices = filter(newData, Tm == selectedTeamB)$Player, 
                           selected = NULL)
  
})

observeEvent(input$tradeButton,{
  
  # update teamPowerA and B by calculating new powers
  newData <- .trade_Players(playersNew, input$tradePlayersA,input$tradeTeamA,input$tradePlayersB,input$tradeTeamB) 
  # update checkboxGroupInput
  updateCheckboxGroupInput(session, 'tradePlayersA', 
                           choices = filter(newData, Tm == input$tradeTeamA)$Player, 
                           selected = NULL)
  updateCheckboxGroupInput(session, 'tradePlayersB', 
                           choices = filter(newData, Tm == input$tradeTeamB)$Player, 
                           selected = NULL)
  
})

# Take a reactive dependency on input$button, but
# not on any of the stuff inside the function
powerA_new <- eventReactive(input$tradeButton, {
  #if (!exists(newData)) newData <- playersNew
  print(paste0("Updating Power for ", input$tradeTeamA))
  newData <- .trade_Players(playersNew, input$tradePlayersA,input$tradeTeamA,input$tradePlayersB,input$tradeTeamB) 
  newPower <- merge(.computePower(newData,"PTS",input$tradeTeamA),.computePower(newData,"PTSA",input$tradeTeamA),by="team_season") %>% 
    as.data.frame()
  newPower <- newPower[,-1]
  names(newPower) <- c("Offense","Defense")  
  print(paste0("New Off: ", newPower[,1], ". New Def: ",newPower[,2]))
  return(newPower)
})
output$teamPowerA_new <- DT::renderDataTable({
  powerA_new()
},options = list(dom = 't'))#, rownames = FALSE)

powerB_new <- eventReactive(input$tradeButton, {
  #if (!exists(newData)) newData <- playersNew
  print(paste0("Updating Power for ", input$tradeTeamB))
  newData <- .trade_Players(playersNew, input$tradePlayersA,input$tradeTeamA,input$tradePlayersB,input$tradeTeamB) 
  newPower <- merge(.computePower(newData,"PTS",input$tradeTeamB),.computePower(newData,"PTSA",input$tradeTeamB),by="team_season") %>% 
    as.data.frame()
  newPower <- newPower[,-1]
  names(newPower) <- c("Offense","Defense")  
  print(paste0("New Off: ", newPower[,1], ". New Def: ",newPower[,2]))
  return(newPower)
})
output$teamPowerB_new <- DT::renderDataTable({
  powerB_new()
},options = list(dom = 't'))#, rownames = FALSE)

# All teams in 1 table
output$teamPower_all <- DT::renderDataTable({
  
  power <- teamsPredicted %>% 
    select(TeamCode, Offense = TEAM_PTS, Defense = TEAM_PTSAG) %>%
    mutate(Offense = round(Offense,2), Defense = round(Defense,2))
  return(power)
},options = list(dom = 't', pageLength = 30))#, rownames = FALSE)

# output$teamPower_all_new <- DT::renderDataTable({
#   newPower <- merge(.computePower(newData,"PTS",input$tradeTeamB),.computePower(newData,"PTSA",input$tradeTeamB),by="team_season") %>% 
#     as.data.frame()
#   newPower <- newPower[,-1]
#   names(newPower) <- c("Offense","Defense")  
#   return(power)
# },options = list(dom = 't'))#, rownames = FALSE)
# 

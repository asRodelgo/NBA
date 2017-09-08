# Get rosters-------

output$teamPowerA <- DT::renderDataTable({
  
  power <- filter(teamsPredicted, TeamCode == input$tradeTeamA) %>% 
    select(Offense = TEAM_PTS, Defense = TEAM_PTSAG)
  return(power)
},options = list(dom = 't'), rownames = FALSE)

output$teamPowerB <- DT::renderDataTable({
  
  power <- filter(teamsPredicted, TeamCode == input$tradeTeamB) %>% 
    select(Offense = TEAM_PTS, Defense = TEAM_PTSAG)
  return(power)
},options = list(dom = 't'), rownames = FALSE)

observeEvent(input$tradeButton,{
  
  # update teamPowerA and B
  
  # update checkboxGroupInput
  
})
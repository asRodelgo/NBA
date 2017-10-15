# simulate multiple seasons
simulate_n_seasons <- function(num_sim = 1) {
  
  # Simulate a few seasons
  regSeasonOutcome <- .standings(real = TRUE)
  # Initialize parameters
  regSeasonAvg2 <- data.frame(
    team = regSeasonOutcome[[1]][[168]]$team,
    teamCode = regSeasonOutcome[[1]][[168]]$teamCode,
    conference = regSeasonOutcome[[1]][[168]]$conference,
    win = 0,
    lose = 0,
    win2 = 0,
    sd = 0,
    probChamp = 0)
  
  num_seasons <- num_sim
  
  for (i in 1:num_seasons){
    
    final_standings <- regSeasonOutcome[[1]][[168]]
    #playoffs <- .getPlayoffResults(final_standings) %>% mutate(round = ifelse(round == 0,1,0)) %>%
    #  group_by(teamCode) %>% summarise(round = sum(round)) %>% ungroup()
    regSeasonAvg2$win <- regSeasonAvg2$win + final_standings$win
    regSeasonAvg2$win2 <- regSeasonAvg2$win2 + (final_standings$win)^2
    #probChamp <- merge(final_standings, playoffs[,c("teamCode","round")],by="teamCode",all.x=TRUE) %>%
    #  mutate(round = ifelse(is.na(round),0,round))
    #regSeasonAvg2$probChamp <- regSeasonAvg2$probChamp + probChamp$round
    # generate a new season outcome
    regSeasonOutcome <- .standings(real = TRUE)
    # keep count
    print(paste0("iteration: ",i))
  }
  
  regSeasonAvg2$win <- regSeasonAvg2$win/num_seasons
  regSeasonAvg2$lose <- 82 - regSeasonAvg2$win
  regSeasonAvg2$win2 <- regSeasonAvg2$win2/num_seasons
  regSeasonAvg2$sd <- sqrt(regSeasonAvg2$win2 - (regSeasonAvg2$win)^2)
  
  return(regSeasonAvg2)
}
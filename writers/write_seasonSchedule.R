# Real season schedule from basketball-reference

.write_SeasonSchedule <- function(){
  
  library(httr)
  library(rvest)
  
  # If not new data yet (transfers not finished so teams rosters not final), -----------------------------------------
  dataNewSeason <- FALSE
  if (dataNewSeason==FALSE){
    # use last season's as new data, removing PTS & PTSA
    team_statsNew <- team_stats %>%
      filter(Season == max(as.character(Season))) %>%
      mutate(W = 0, L = 0, PTS = 0, PTSA = 0, SRS = 0, 
             Season = paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)) %>%
      distinct(Team, .keep_all=TRUE)
    # same for players
    playersNew <- playersHist %>%
      filter(Season == max(as.character(Season))) %>%
      mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
  }
  
  thisSeason <- substr(as.character(playersNew$Season[1]),6,9)
  months_list <- c("october","november","december",'january',"february","march","april")
  
  season_schedule <- data.frame()
  for (month in months_list){
    
    url <- paste0("http://www.basketball-reference.com/leagues/NBA_",
                  thisSeason,"_games-",month,".html")
    
    thisMonthSchedule <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="schedule"]') %>%
      html_table(fill = TRUE)
    thisMonthSchedule <- thisMonthSchedule[[1]]
    season_schedule <- bind_rows(season_schedule,thisMonthSchedule)
  }
  season_schedule <- dplyr::select(season_schedule, Date,StartTime=`Start (ET)`,
                                  teamH=`Home/Neutral`, teamA=`Visitor/Neutral`)
  
  
  # Convert team names to team codes
  season_schedule <- merge(season_schedule,team_statsNew[,c("Team","teamCode")],by.x="teamH",by.y="Team",all.x=TRUE)
  season_schedule <- merge(season_schedule,team_statsNew[,c("Team","teamCode")],by.x="teamA",by.y="Team",all.x=TRUE)
  season_schedule <- season_schedule %>%
    dplyr::select(-teamA,-teamH, teamH = teamCode.x, teamA = teamCode.y) %>%
    mutate(Date = paste0(substr(Date,nchar(Date)-3,nchar(Date)),"-",
                         substr(Date,6,8),"-",substr(Date,10,11))) %>%
    mutate(Date = ifelse(grepl(",",Date),paste0(substr(Date,1,nchar(Date)-2),"0",
                                                substr(Date,nchar(Date)-1,nchar(Date)-1)),Date)) %>%
    mutate(Date = as.Date(Date,"%Y-%B-%d"), 
           StartTime = ifelse(nchar(StartTime)<8,paste0("0",StartTime),StartTime)) %>%
    arrange(Date,StartTime)
    
  write.csv(season_schedule,"data/realSeasonSchedule.csv",row.names = FALSE)
}


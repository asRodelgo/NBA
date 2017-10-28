# Make changes to teams rosters in 2 ways:
# A. Same roster, minutes adjustments (eg: in playoffs star players take bigger share of minutes)
# B. Changes in roster: drafted players, aging players, leaving players, etc.

# A. Minutes adjustments
# start with playersNew
# playersNew <- playersHist %>%
#   filter(Season == max(as.character(Season))) %>%
#   mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
.adjust_Minutes <- function(data,increment,topHeavy = 5,convertEffMin = FALSE){
  #increment <- 0.2  
  if (convertEffMin) { # transform effMin back to MP per game
    data <- mutate(data, MP = effMin*82*48)
  }
  minAdjust <- filter(data,!(Tm == "TOT"))
  playersAdj <- data.frame()  
  for (team in unique(minAdjust$Tm)) {
    atl <- filter(minAdjust, Tm == team) %>% arrange(desc(MP))
    total_min <- sum(atl$MP)
    leftout_min <- sum(atl$MP[(topHeavy+1):nrow(atl)])
    # top 5 highest minutes played increase their play time by x% 
    # Set a limit of 46 out of 48 max minutes per game per player
    atl$MP[1:topHeavy] <- ifelse(atl$MP[1:topHeavy]*(1+increment)>46,46,atl$MP[1:topHeavy]*(1+increment))
    # adjust the rest to sum up to total_min
    increm_min <- sum(atl$MP[1:topHeavy])
    leftout_coef <- (total_min-increm_min)/leftout_min
    atl$MP[(topHeavy+1):nrow(atl)] <- leftout_coef*atl$MP[(topHeavy+1):nrow(atl)]
    if (nrow(playersAdj)>0) playersAdj <- bind_rows(playersAdj,atl) else playersAdj <- atl
    
    #print(round(total_min,1) == round(sum(atl$MP),1))
  } 
  
  if (convertEffMin) { # transform effMin back to MP per game
    data <- mutate(data, effMin = MP/(82*48)) %>% select(-MP)
  }
  
  return(playersAdj)
  
}

.convertMin2Percent <- function(data){
  
  data <- group_by(data, Tm) %>% 
    mutate(TotEffMin = sum(effMin)) %>%
    ungroup() %>%
    mutate(effMin = effMin/TotEffMin) %>%
    select(-TotEffMin) %>%
    data.frame()
  
  return(data)
}

.redistributeMinutes <- function(data,topHeavy = 7,topMinShare = .6, min_share_top1 = .1){
  # Usually a few players amasse a great amount of the minutes. 
  # Ex: Heuristically, top 7 players play 60% of total time
  playersAdj <- data.frame()  
  for (team in unique(data$Tm)) {
    # introduce jitter as some players have the same effMin (they were averaged out at the prediction phase)
    jitter <- runif(nrow(filter(data, Tm == team)))/100000
    atl <- filter(data, Tm == team) %>% arrange(desc(effMin)) %>%
      as.data.frame() %>%
      mutate(effMin = effMin + jitter) 
    bottomHeavy <- nrow(atl)-topHeavy
    total_min <- sum(atl$effMin)
    topHeavy_min <- sum(atl$effMin[1:(topHeavy)])
    topHeavy_min_target <- total_min*topMinShare
    delta_min <- topHeavy_min_target - topHeavy_min
    # # adjust the rest to sum up to total_min
    atl_top <- top_n(atl, topHeavy, effMin) %>%
      mutate(effMin = effMin + delta_min/topHeavy)
    atl_bottom <- top_n(atl, bottomHeavy, -effMin) %>%
      mutate(effMin = effMin - delta_min/bottomHeavy)
    atl <- rbind(atl_top, atl_bottom)
    # check results
    #sum(atl$effMin[1:(topHeavy)])/total_min
    #sum(atl$effMin[(topHeavy+1):nrow(atl)])/total_min
    
    # double check cases in which after adjustment a player's minutes go beyond the realistic (limit_time_player)
    # No player can have more than 10.5% (or other value provided as parameter) of total team play time (empirically per .minutesDensity())
    player_time_limit <- total_min*min_share_top1
    overplay <- 0
    atl$overplay <- 0
    for (i in 1:topHeavy) {
      if (atl$effMin[i] > player_time_limit) {
        overplay <- overplay + atl$effMin[i] - player_time_limit
        atl$effMin[i] <- player_time_limit
        atl$overplay[i] <- 1 # this player surpassed minutes allowed
      }
    }
    # distribute the extra minutes among all players (except those exceeding)
    overplay_share <- overplay/nrow(filter(atl, overplay == 0))
    atl <- mutate(atl, effMin = ifelse(overplay == 0, effMin+overplay_share,effMin)) %>% 
      select(-overplay)
    # check results
    #sum(atl$effMin[1:(topHeavy)])/total_min
    #sum(atl$effMin[(topHeavy+1):nrow(atl)])/total_min
    
    # run the adjustment again until settled
    iter <- 1 # avoid infinite loops
    while (abs(delta_min) > .0001 & iter <= 10) {
      
      total_min <- sum(atl$effMin)
      topHeavy_min <- sum(atl$effMin[1:(topHeavy)])
      topHeavy_min_target <- total_min*topMinShare
      delta_min <- topHeavy_min_target - topHeavy_min
      # # adjust the rest to sum up to total_min
      atl_top <- top_n(atl, topHeavy, effMin) %>%
        mutate(effMin = effMin + delta_min/topHeavy)
      atl_bottom <- top_n(atl, bottomHeavy, -effMin) %>%
        mutate(effMin = effMin - delta_min/bottomHeavy)
      atl <- rbind(atl_top, atl_bottom)
      #
      player_time_limit <- total_min*min_share_top1
      overplay <- 0
      atl$overplay <- 0
      for (i in 1:topHeavy) {
        if (atl$effMin[i] > player_time_limit) {
          overplay <- overplay + atl$effMin[i] - player_time_limit
          atl$effMin[i] <- player_time_limit
          atl$overplay[i] <- 1 # this player surpassed minutes allowed
        }
      }
      # distribute the extra minutes among all players (except those exceeding)
      overplay_share <- overplay/nrow(filter(atl, overplay == 0))
      atl <- mutate(atl, effMin = ifelse(overplay == 0, effMin+overplay_share,effMin)) %>% 
        select(-overplay)
      
      iter <- iter + 1
    }
    
    if (nrow(playersAdj)>0) playersAdj <- bind_rows(playersAdj,atl) else playersAdj <- atl
  }
  # add jitter to the final effMin to avoid duplicated effMin
  jitter <- runif(nrow(playersAdj))/100000
  playersAdj <- mutate(playersAdj, effMin = effMin + jitter)
  
  return(playersAdj)
}
# Trade players
# data <- playersNew
# playA <- "Paul George"
# playB <- "Edy Tavares"
# tmA <- "IND"
# tmB <- "CLE"
.trade_Players <- function(data,playA,tmA,playB=NULL,tmB=NULL){
  
  if (is.null(tmB)) { # player is traded out of NBA or retires
    
    #playerA_row <- filter(data, Player %in% playA, Tm == tmA)
    data <- filter(data, !(Player %in% playA))
    
  } else if (is.null(playB)) {  # playerA is traded to teamB
    
    playerA_row <- filter(data, Player %in% playA, Tm == tmA) %>% mutate(Tm = tmB)
    data <- filter(data, !(Player %in% c(playA))) %>% 
      bind_rows(playerA_row)
    
  } else { # trade between 2 NBA teams
    
    playerA_row <- filter(data, Player %in% playA, Tm == tmA) %>% mutate(Tm = tmB)
    playerB_row <- filter(data, Player %in% playB, Tm == tmB) %>% mutate(Tm = tmA)
    data <- filter(data, !(Player %in% c(playA, playB))) %>% 
      bind_rows(playerA_row) %>% bind_rows(playerB_row)  
  
  }
    
  return(data)
  
}

# injured player
.disabled_list <- function(data,player_list) {
  
  data <- mutate(data, effMin = ifelse(Player %in% player_list, 0, effMin))
  
  return(data)
  
}

# Calculate the average player
.calculate_AvgPlayer <- function(data, age=NULL) {
  thisSeason <- data$Season[1]
  if (is.null(age)){ # average all
    avgPlayer <- summarise_if(data,is.numeric,funs(mean)) %>% 
      mutate(Player = "Average Player", Pos = "X", Tm = "X", Season = thisSeason) %>%
      select(Player,Pos,Age,Tm,everything())
  } else { # average by age
    avgPlayer <- filter(data, Age == age) %>% 
      summarise_if(is.numeric,funs(mean)) %>% 
      mutate(Player = "Average Player", Pos = "X", Tm = "X", Season = thisSeason) %>%
      select(Player,Pos,Age,Tm,everything())
  }
  
  
  return(avgPlayer)
}

# Calculate average distribution of minutes for top 1,2,3,...10 players to help adjust minutes before prediction
.minutes_density <- function(data, seasons = 5) {
  
    data_team <- data %>%
      filter(Season %in% top_n(distinct(data,Season),seasons)$Season, !(Tm == "TOT")) %>% # take last 5 seasons as sample
      group_by(Player) %>%
      mutate(effMin = MP/3936) %>%
      select(Player,Season,Tm,effMin)
    
    # Adjust effMin to reflect percentage of total team time played, ie, sum(effMin) = 5
    data_team2 <- data_team %>%
      group_by(Tm,Season) %>%
      mutate(effMin = effMin/sum(effMin,na.rm=TRUE)) %>%
      arrange(Season,Tm,desc(effMin)) %>%
      top_n(18,effMin) %>%
      mutate(top18 = sum(effMin)) %>%
      top_n(17,effMin) %>%
      mutate(top17 = sum(effMin)) %>%
      top_n(16,effMin) %>%
      mutate(top16 = sum(effMin)) %>%
      top_n(15,effMin) %>%
      mutate(top15 = sum(effMin)) %>%
      top_n(14,effMin) %>%
      mutate(top14 = sum(effMin)) %>%
      top_n(13,effMin) %>%
      mutate(top13 = sum(effMin)) %>%
      top_n(12,effMin) %>%
      mutate(top12 = sum(effMin)) %>%
      top_n(11,effMin) %>%
      mutate(top11 = sum(effMin)) %>%
      top_n(10,effMin) %>%
      mutate(top10 = sum(effMin)) %>%
      top_n(9,effMin) %>%
      mutate(top9 = sum(effMin)) %>%
      top_n(8,effMin) %>%
      mutate(top8 = sum(effMin)) %>%
      top_n(7,effMin) %>%
      mutate(top7 = sum(effMin)) %>%
      top_n(6,effMin) %>%
      mutate(top6 = sum(effMin)) %>%
      top_n(5,effMin) %>%
      mutate(top5 = sum(effMin)) %>%
      top_n(4,effMin) %>%
      mutate(top4 = sum(effMin)) %>%
      top_n(3,effMin) %>%
      mutate(top3 = sum(effMin)) %>%
      top_n(2,effMin) %>%
      mutate(top2 = sum(effMin)) %>%
      top_n(1,effMin) %>%
      mutate(top1 = sum(effMin)) %>%
      distinct(Season, Tm, .keep_all=TRUE) %>%
      ungroup() %>%
      select(-c(Player,effMin)) %>%
      as.data.frame()
    
    return(data_team2)
}




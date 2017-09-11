# Make changes to teams rosters in 2 ways:
# A. Same roster, minutes adjustments (eg: in playoffs star players take bigger share of minutes)
# B. Changes in roster: drafted players, aging players, leaving players, etc.

# A. Minutes adjustments
# start with playersNew
# playersNew <- playersHist %>%
#   filter(Season == max(as.character(Season))) %>%
#   mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
.adjust_Minutes <- function(data,increment,topHeavy = 5){
  #increment <- 0.2  
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
  return(playersAdj)
  
}

# Trade players
# data <- playersNew
# playA <- "Paul George"
# playB <- "Edy Tavares"
# tmA <- "IND"
# tmB <- "CLE"
.trade_Players <- function(data,playA,tmA,playB=NULL,tmB=NULL){
  
  if (is.null(playB) | is.null(tmB)) { # player is traded out of NBA or retires
    
    playerA_row <- filter(data, Player %in% playA, Tm == tmA) %>% mutate(Tm = tmB)
    data <- filter(data, !(Player %in% playA)) %>% 
      bind_rows(playerA_row)
    
  } else {  # trade between 2 NBA teams
    
    playerA_row <- filter(data, Player %in% playA, Tm == tmA) %>% mutate(Tm = tmB)
    playerB_row <- filter(data, Player %in% playB, Tm == tmB) %>% mutate(Tm = tmA)
    data <- filter(data, !(Player %in% c(playA, playB))) %>% 
      bind_rows(playerA_row) %>% bind_rows(playerB_row)
  }
    
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

# draft player

# update players skills due to aging






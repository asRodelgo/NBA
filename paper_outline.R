##### Paper Outline -------------------------------
# This script will describe step by step data preparation needed to get intended outcomes
# Date: October 1, 2017
##### ---------------------------------------------
## Step 1: Take rosters at the end of last season. They will contain regular season stats. 
# Use this as the baseline to predict their stats for the upcoming season. 
# Then take all the rookies from the rosters as of today (trades may still happen before the regular season
# gets started but those can be dealt with using tradePlayers functions). With these rookies, calculate
# their stats from their college years (last 3 seasons) or European league stats (all available seasons). 
# Leftovers from these two categories are averaged out. This results in: rookieStats (see write_rookiesDraft.R)

# Global utils -----------------------------
library(tidyverse)
library(tsne)
library(rvest)
library(rlist) # save and load list objects
library(scales)

thisYear <- substr(Sys.Date(),1,4)
if (substr(Sys.Date(),6,7) > '08'){
  seasonOffset <- 0
} else {
  seasonOffset <- 1
}
thisSeason <- paste0(as.numeric(substr(Sys.Date(),1,4))-seasonOffset,"-",as.numeric(substr(Sys.Date(),1,4))-seasonOffset+1) 
# Source all files from server_files directory and subdirectories
files <- list.files("helper_functions", full.names = TRUE, recursive = TRUE)
for (f in files) source(f, local = TRUE)

# Read pre-calculated tSNE coordinates per Age -----------------
# from write_tsneBlocks.R
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- read.csv(paste0("data/tsneBlock","_",a,".csv"))
}

# Read team stats for all seasons ------------------------------
team_stats <- read.csv("data/teamStats.csv") # from write_TeamStats.R

## These data may not change from season to season
franchises <- read.csv("data/franchisesHistory.csv",stringsAsFactors = FALSE) # Manually obtained. No need to change unless there are changes in team names or new teams get added to the league
team_stats <- merge(team_stats,franchises,by.x="Team",by.y="Franchise",all.x=TRUE)
# conferences according to last season
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE) # Same as franchises 

# Read pre-calculated nnetwork models -------------------------
#nn_Offense <- list.load("data/nn_Offense.rds")
#nn_Defense <- list.load("data/nn_Defense.rds")
load("data/modelNeuralnet2_PTS.Rdata")
nn_Offense <- model$finalModel
load("data/modelNeuralnet2_PTSA.Rdata")
nn_Defense <- model$finalModel

# Actual Season schedule
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE) # from write_seasonSchedule.R
datesRange <- unique(realSeasonSchedule$Date)

# Global hyperparameters for Normal distributions
global_mean <- mean(team_stats$PTS)
sigma <- 8 # constant std dev for all teams. ADJUST LATER ON!!

playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE) # read historical players from write_playersHist.R
playersHist <- .rename_PlayerDuplicates(playersHist) # differentiate different players with the same name


########################################
# 1. calculate playersNewPredicted

playersNew <- playersHist %>% # keep only players last season
  filter(Season == max(as.character(Season))) %>%
  mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))

playersNewPredicted <- read.csv("data/playersNewPredicted.csv", stringsAsFactors = FALSE) # from .computePredictedPlayerStats() in write_teams_predicted_stats_new_season.R

# 2. Merge with current_rosters into playersNewPredicted_Current
playersNewPredicted_Current <- .mergePredictedWithCurrent() # from write_teams_predicted_stats_new_season.R

# 3. Complete playersNewPredicted with names not matching in current_rosters. Ex: Tim Hardaway vs. Tim Hardaway 2
current_rosters <- read.csv("data/rostersLastSeason.csv", stringsAsFactors = FALSE) # .getLatestRosters from write_teams_predicted_stats_new_season.R
playersMatch <- merge(current_rosters,playersNewPredicted, by = "Player", all.x = TRUE) %>%
  distinct(Player)

playersNonMatch <- filter(playersNewPredicted_Current, !(Player %in% playersMatch$Player))

playersManuallyChanged <- filter(playersNonMatch, Player %in% c(
  "Mike Dunleavy 2","Taurean Waller-Prince","Tim Hardaway 2","Nene Hilario","Glenn Robinson 2",
  "Gary Payton 2","Gerald Henderson 2","Kelly Oubre"
))

playersManuallyChanged <- filter(playersManuallyChanged, !(grepl("Dunleavy",Player) & !(Tm =="ATL") )) # now remove those with several teams:
playersNewPredicted_Current <- filter(playersNewPredicted_Current, !is.na(Exp)) # now remove those non-matching from playersNewPredicted
playersNewPredicted_Current <- bind_rows(playersNewPredicted_Current,playersManuallyChanged) # and add the manually changed to match non-matching players
playersNewPredicted_Current <- select(playersNewPredicted_Current, -c(Exp,College))

# 4. Add rookieStats to complete rosters for new season
playersNewPredicted_Current_All <- bind_rows(playersNewPredicted_Current,rookieEffStats) %>%
  mutate(historical_name = Player)
    # avoid inconsistencies with rookie players vs veterans
    # compare_players <- filter(playersNewPredicted_Current_All, Player %in% c("Lonzo Ball","Milos Teodosic","Kyrie Irving","Ricky Rubio","John Wall"))
# now update rosters to reflect current. 
current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE)
playersNonMatch <- merge(playersNewPredicted_Current_All, by = "Player", 
                         select(current_rosters,Player,Tm,Exp), all.x = TRUE) %>% 
  filter(is.na(Exp))
# some players will not be matched: Different Player name spelling or Players returning to NBA who didn't play last season (that is their Exp != "R")
playersNonMatch <- playersNonMatch$Player
# historical names
# [1] "Adreian Payne"         "Alan Anderson"         "Alex Hamilton"         "Alex Poythress"       
# [5] "Andrew Nicholson"      "Antonius Cleveland"    "Axel Toupane"          "Boris Diaw"           
# [9] "Brandon Bass"          "Brandon Jennings"      "Brian Roberts"         "C.J. Watson"          
# [13] "Chasson Randle"        "Christian Wood"        "Dahntay Jones"         "Darrun Hilliard"      
# [17] "David Lee"             "DeAndre Liggins"       "Demetrius Jackson"     "Deron Williams"       
# [21] "Derrick Williams"      "Donatas Motiejunas"    "Gary Payton 2"         "Gerald Henderson 2"   
# [25] "Glenn Robinson 2"      "JaCorey Williams"      "James Jones"           "James Michael McAdoo" 
# [29] "Jordan Hill"           "Justin Hamilton"       "Kelly Oubre"           "Kevin Seraphin"       
# [33] "Lavoy Allen"           "Leandro Barbosa"       "Matt Barnes"           "Maurice Ndour"        
# [37] "Metta World Peace"     "Mike Dunleavy 2"       "Mike Miller"           "Monta Ellis"          
# [41] "Nene Hilario"          "Norris Cole"           "Patricio Garino"       "Paul Pierce"          
# [45] "Rakeem Christmas"      "Randy Foye"            "Ronnie Price"          "Roy Hibbert"          
# [49] "Ryan Kelly"            "Sasha Vujacic"         "Sergio Rodriguez"      "Shawn Long"           
# [53] "Sheldon McClellan"     "Spencer Hawes"         "Taurean Waller-Prince" "Thomas Robinson"      
# [57] "Tiago Splitter"        "Tim Hardaway 2"        "Trey Burke"            "Ty Lawson" 

# current names
#      c("Andrew Bogut"   ,    "Andy Rautins"       ,"Anthony Bennett"  ,  "Bronson Koenig"     ,"Carrick Felix",     
#       "Chris Johnson"  ,    "Cliff Alexander"    ,"Damien Wilkins"  ,   "Darius Miller"     , "DeQuan Jones" ,     
#       "Donald Sloan"    ,   "Dwight Howard"      ,"Ekpe Udoh"       ,   "Emeka Okafor"      , "Eric Moreland" ,    
#       "Gary Payton II"   ,  "Glenn Robinson III" ,"Jarell Eddie"    ,   "Jarrett Jack"      , "Jeremy Evans"   ,   
#       "John Jenkins"      , "Josh Childress"     ,"Julyan Stone"     ,  "Kalin Lucas"       , "Kelly Oubre Jr.",   
#       "Kendall Marshall"  , "Kendrick Perkins"   ,"LaDontae Henton" ,   "Larry Drew II"     , "Maalik Wayns"    ,  
#       "Marco Belinelli"   , "Marcus Williams"    ,"Mario Chalmers"   ,  "Markel Brown"      , "Mike Scott"  ,      
#       "Miles Plumlee"     , "Nene"               ,"Perry Jones"     ,   "Quincy Pondexter"  , "Shane Larkin",      
#       "Sheldon Mac"       , "Taurean Prince"     ,"Tim Frazier"     ,   "Tim Hardaway"      , "Vander Blue"),
# action = c("compute","compute","compute","compute","compute",
#            "compute","compute","compute","compute","compute",
#            "compute","compute","compute","compute","compute",
#            "Gary Payton 2","Glenn Robinson 2","compute","compute","compute",
#            "compute","compute","compute","compute","Kelly Oubre",
#            "compute","compute","compute","compute","compute",
#            "compute","compute","compute","compute","compute",
#            "compute","Nene Hilario","compute","compute","compute",
#            "compute","Taurean Waller-Prince","compute","Tim Hardaway 2","compute")
#    )
current_rosters[which(current_rosters$Player == "Gary Payton II"),]$Player <- "Gary Payton 2"
current_rosters[which(current_rosters$Player == "Glenn Robinson III"),]$Player <- "Glenn Robinson 2"
current_rosters[which(current_rosters$Player == "Kelly Oubre Jr."),]$Player <- "Kelly Oubre"
current_rosters[which(current_rosters$Player == "Nene"),]$Player <- "Nene Hilario"
current_rosters[which(current_rosters$Player == "Taurean Prince"),]$Player <- "Taurean Waller-Prince"
current_rosters[which(current_rosters$Player == "Tim Hardaway"),]$Player <- "Tim Hardaway 2"
# merge current and already predicted from historical file filtered by last season
playersNewPredicted_Current_All <- merge(select(current_rosters,Player,Tm,Exp),
                                         playersNewPredicted_Current_All, by = "Player", all.x = TRUE)
unmatched_Players <- filter(playersNewPredicted_Current_All, is.na(Tm.y))
# compute predicted stats for unmatched players (they played in NBA at some point but don't have stats for last season)
# make sure they're all in playersHist in case manual edits are needed for players names
# See top of script for how playersHist get read
playersNewLeftover <- filter(playersHist, Player %in% unmatched_Players$Player) %>% 
  arrange(Player,desc(Season)) %>%
  distinct(Player, .keep_all=TRUE)
unmatched_Players_Leftover <- filter(unmatched_Players, !(Player %in% playersNewLeftover$Player))$Player
# "Bronson Koenig"  "LaDontae Henton" "Larry Drew II"   "Sheldon Mac"
unmatched_Players[which(unmatched_Players$Player == "Larry Drew II"),]$Player <- "Larry Drew 2"
unmatched_Players[which(unmatched_Players$Player == "Sheldon Mac"),]$Player <- "Sheldon McClellan"
#
playersNewLeftover <- filter(playersHist, Player %in% unmatched_Players$Player) %>% 
  arrange(Player,desc(Season)) %>%
  distinct(Player, .keep_all=TRUE)
unmatched_Players_Leftover <- filter(unmatched_Players, !(Player %in% playersNewLeftover$Player))
# "Bronson Koenig"  "LaDontae Henton"    
playersNewLeftover2 <- merge(select(playersNewLeftover, -Tm),
                            select(unmatched_Players, Player, Tm = Tm.x),
                            by = "Player", all.y = TRUE)
playersNewLeftover2 <- mutate(playersNewLeftover2, Season = paste0(as.numeric(thisSeason)-1,"-",thisSeason),
                              Age = ifelse(is.na(Age),25,Age)) # arbitrarily assign Age = 25 for those missing Age
# calculate predicted stats for these leftovers
playersNewLeftover3 <- .computePredictedPlayerStats_Leftovers(playersNewLeftover2)
# Merge with the rest of predicted players to complete rosters
playersNewPredicted_Current_All <- filter(playersNewPredicted_Current_All, !is.na(effPTS)) %>%
  mutate(Tm = Tm.x) %>%
  select(-Tm.x,-Tm.y,-Exp,-historical_name)
playersNewPredicted_Final <- rbind(playersNewPredicted_Current_All,playersNewLeftover3)
write.csv(playersNewPredicted_Final, "data/playersNewPredicted_Final.csv",row.names = FALSE)

# load pre-calculated final players predictions. effMin are not adjusted, i.e., rookies will have higher
# effMin than it would be expected.
playersNewPredicted_Final <- read.csv("data/playersNewPredicted_Final.csv",stringsAsFactors = FALSE)
# adjust players minutes. Reduce new players effMin (rookies, international, returning) by 30%
playersNewPredicted_Final_adjMin <- mutate(playersNewPredicted_Final,
                                           effMin = ifelse(is.na(Age),effMin*.7,effMin))
# adjust percent of play time (change this to use empirical data from past seasons)
# Empirically:
topMinShare <- .minutes_density(playersHist,10)
averageShare <- group_by(topMinShare,Season) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  summarise_if(is.numeric, mean)
incrementShare <- gather(averageShare,top_n,percent)
plot(seq(13,1,-1),incrementShare$percent)
# top7 seems to be the turning point at 60%, after it, the scale of time every new player adds goes down (slope). 
# I will use this as estimate. Although the trend is going down, in last 5 seasons, % is 59%
# because rosters are getting bigger thus utilize more players. 
playersNewPredicted_Final_adjMin2 <- .redistributeMinutes(playersNewPredicted_Final_adjMin,topHeavy = 7, topMinShare = .6)
# make sure Season column shows the new season to come
playersNewPredicted_Final_adjMin2 <- mutate(playersNewPredicted_Final_adjMin2, Season = paste0(thisYear,"-",as.numeric(thisYear)+1))

# compute team powers. See teams_power.R for details
#effMinutes <- NULL # approx the average of all 
#teamPowers_newSeason <- merge(.computePower(playersNewPredicted_Final_adjMin2,"PTS","All",effMinutes,actualOrPredicted = "predicted"),.computePower(playersNewPredicted_Final_adjMin2,"PTSA","All",effMinutes,actualOrPredicted = "predicted"),by="team_season")
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Final_adjMin2,actualOrPred="predicted")

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

num_seasons <- 10

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


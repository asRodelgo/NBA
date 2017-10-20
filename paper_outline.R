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
#load("data/modelNeuralnet2_PTS.Rdata")
load("data/modelNeuralnet4_PTS.Rdata")
nn_Offense <- model$finalModel
#load("data/modelNeuralnet2_PTSA.Rdata")
load("data/modelNeuralnet4_PTSA.Rdata")
nn_Defense <- model$finalModel

# Actual Season schedule
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE) # from write_seasonSchedule.R
datesRange <- unique(realSeasonSchedule$Date)

### Global hyperparameters for Normal distributions
# get game scores from past 7 seasons (since 2010)
gameScores <- read.csv("data/gameScores.csv", stringsAsFactors = FALSE) # write_scoreDifferentials.R 
#sigmaHome <- sd(as.numeric(gameScores$pts_home), na.rm = TRUE)
#sigmaAway <- sd(as.numeric(gameScores$pts_away), na.rm = TRUE)
sigma <- sd(c(as.numeric(gameScores$pts_home),as.numeric(gameScores$pts_away)), na.rm = TRUE)
avgHome <- mean(as.numeric(gameScores$pts_home), na.rm = TRUE)
avgAway <- mean(as.numeric(gameScores$pts_away), na.rm = TRUE)
global_mean <- mean(c(as.numeric(gameScores$pts_home),as.numeric(gameScores$pts_away)), na.rm = TRUE)
home_away_factor <- avgHome - avgAway # how many extra points does a team score on average when playing home

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
playersNewLeftover2 <- mutate(playersNewLeftover2, Season = paste0(as.numeric(thisYear),"-",as.numeric(thisYear)+1),
                              Age = ifelse(is.na(Age),25,Age)) # arbitrarily assign Age = 25 for those missing Age
# calculate predicted stats for these leftovers
playersNewLeftover3 <- .computePredictedPlayerStats_Leftovers(playersNewLeftover2)
# Merge with the rest of predicted players to complete rosters
playersNewPredicted_Current_All <- filter(playersNewPredicted_Current_All, !is.na(effPTS)) %>%
  mutate(Tm = Tm.x) %>%
  select(-Tm.x,-Tm.y,-Exp,-historical_name)
playersNewPredicted_Final <- rbind(playersNewPredicted_Current_All,playersNewLeftover3) %>%
  mutate(Season = paste0(as.numeric(thisYear),"-",as.numeric(thisYear)+1)) %>%
  distinct(Player,Tm, .keep_all=TRUE)
write.csv(playersNewPredicted_Final, "data/playersNewPredicted_Final.csv",row.names = FALSE)

# load pre-calculated final players predictions. effMin are not adjusted, i.e., rookies will have higher
# effMin than it would be expected.
## NOTE: THIS LOCKS ROSTERS AS OF OCTOBER 10 2017. FURTHER CHANGES IN ROSTERS I WILL MAKE VIA MANUAL 
## TRANSFERS UNTIL OCTOBER 17 WHEN THE SEASON STARTS
playersNewPredicted_Final <- read.csv("data/playersNewPredicted_Final.csv",stringsAsFactors = FALSE)
# adjust players minutes. Reduce new players effMin (rookies, international, returning) by x%
# estimate empirically the % reduction in effMin
collegePlayersHist <- read.csv("data/collegePlayersHist.csv", stringsAsFactors = FALSE)
# compute % minutes played per player:
collegeMinutes <- select(collegePlayersHist,Rk,Player,Season,School,G,MP) %>%
  mutate(perMin = MP/(G*40)) %>%
  group_by(Player) %>%
  mutate(perMin = mean(perMin, na.rm=TRUE)) %>% # average minutes played per game in ther college years
  distinct(Player, .keep_all=TRUE) %>%
  filter(!is.na(perMin))
# now do the same for NBA players by experience year
nbaMinutes <- select(playersHist, Player,Season,Age,Tm,G,MP) %>%
  filter(!(Tm == "TOT")) %>%
  group_by(Player) %>%
  filter(Age == min(Age)) %>% # keep players when they were younger (rookie year)
  group_by(Player,Season) %>%
  filter(G >= 30) %>% # played at least 30 games
  mutate(perMin = mean(MP, na.rm=TRUE)/48) %>%
  distinct(Player, .keep_all=TRUE)
# put them together
college2nbaMinutes <- merge(nbaMinutes,collegeMinutes, by = "Player", all.x = TRUE) %>%
  mutate(minDiff = perMin.y-perMin.x) %>%
  filter(!is.na(minDiff))
# estimate difference in minutes played:
col2nbaMinDiff <- mean(college2nbaMinutes$minDiff)
playersNewPredicted_Final_adjMin <- mutate(playersNewPredicted_Final,
                                           effMin = ifelse(is.na(Age),effMin*(1-col2nbaMinDiff),effMin))

# adjust percent of play time
# Based on historical data for the last 5 seasons:
topMinShare <- .minutes_density(playersHist,5)
averageShare <- group_by(topMinShare,Season) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  summarise_if(is.numeric, mean)
incrementShare <- gather(averageShare,top_n,percent)
plot(seq(18,1,-1),incrementShare$percent)
# top7 seems to be the turning point at 60%, after it, the scale of time every new player adds goes down (slope). 
# I will use this as estimate. Although the trend is going down, in last 5 seasons, % is 59%
# because rosters are getting bigger thus utilize more players. Usually top 1 % revolves around 10% 
#playersNewPredicted_Final_adjMin2 <- .redistributeMinutes(playersNewPredicted_Final_adjMin, topHeavy = 11, topMinShare = .8, min_share_top1 = .11)
playersNewPredicted_Final_adjMin2 <- .redistributeMinutes(playersNewPredicted_Final_adjMin, topHeavy = 3, topMinShare = .3, min_share_top1 = .11)
playersNewPredicted_Final_adjMin2 <- .redistributeMinutes(playersNewPredicted_Final_adjMin2, topHeavy = 7, topMinShare = .6, min_share_top1 = .11)
# The reason to go top_1 = 1.1 is to give more prominence to star players which adjust better when simulating
# wins in the regular season
# make sure Season column shows the new season to come
playersNewPredicted_Final_adjMin2 <- mutate(playersNewPredicted_Final_adjMin2, Season = paste0(thisYear,"-",as.numeric(thisYear)+1))

# compute team powers. See teams_power.R for details
# see if actual effMin matter (double check weighted means)
# playersNewPredicted_pumped <- mutate(playersNewPredicted_Final_adjMin2, effMin = ifelse(Tm == "UTA",effMin - .002,effMin))
# teamsPredicted_pumped <- .teamsPredictedPower(data = playersNewPredicted_pumped,actualOrPred="predicted")
# confrmed: effMin volume matters, I will transform in percentages
playersNewPredicted_Final_adjMinPer <- group_by(playersNewPredicted_Final_adjMin2, Tm) %>%
  mutate(effMin = effMin/sum(effMin,na.rm=TRUE)) %>%
  as.data.frame()
# Check percentage of minutes distribution matches the actuals from incrementShare
topMin <- 4
topX <- arrange(playersNewPredicted_Final_adjMinPer, desc(effMin)) %>%
  group_by(Tm) %>%
  top_n(topMin,effMin) %>%
  summarise(sum(effMin))
# topMinY <- 7                
# topY <- arrange(playersNewPredicted_Final_adjMinPer, desc(effMin)) %>%
#   group_by(Tm) %>%
#   top_n(topMinY,effMin) %>%
#   summarise(sum(effMin))
# there's some disparity that will affect the model. I'd rather have all teams's minutes
# equally distributed. ToDo: Fix effMin total = 30% for topHeavy in [8,13] range



#effMinutes <- NULL # approx the average of all 
#teamPowers_newSeason <- merge(.computePower(playersNewPredicted_Final_adjMin2,"PTS","All",effMinutes,actualOrPredicted = "predicted"),.computePower(playersNewPredicted_Final_adjMin2,"PTSA","All",effMinutes,actualOrPredicted = "predicted"),by="team_season")

# Use 6-4-2 nnets (modelNeuralnet4_PTS.Rdata)
teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Final_adjMinPer,actualOrPred="predicted")
# make sure total PTS scored = total PTS against, although this won't change anything in win/loss predictions
teamsPredicted <- mutate(teamsPredicted, TEAM_PTSAG = TEAM_PTSAG + (sum(TEAM_PTS)-sum(TEAM_PTSAG))/nrow(teamsPredicted))
# teamsPredicted <- mutate(teamsPredicted, basketAverage = TEAM_PTS - TEAM_PTSAG)
# simulate a few seasons:
win_predictions <- simulate_n_seasons(10)
# Some unexpected results. Things to consider:
# rookies stat prediction transition from college/international
# the way I average players when not sufficient data on them
# make sure there are not outliers, or similar
# The way game scores are computed. Home/Away advantage. Factor in the distance between cities or conferences

# create a status column to adjust for players injuries. Example: 
# Isaiah Thomas will most likely miss 1/3 of the regular season. Then his status becomes: .66
# I then adjust effMin = effMin*status
player_injury_status <- data.frame(Player = c("Isaiah Thomas","Nicolas Batum","Gordon Hayward","Jeremy Lin"),
                                   status = c(.66,.66,.0,.0))
# adjust rosters per injuries
player_predictions <- merge(playersNewPredicted_Final_adjMinPer,player_injury_status, by="Player", all.x = TRUE) %>%
  mutate(status = ifelse(is.na(status),1,status)) %>%
  mutate(effMin = effMin*status)
# recalculate teamsPredicted. First reassign minutes and then predict
player_predictions_DL_adj <- .redistributeMinutes(player_predictions, topHeavy = 7, topMinShare = .6, min_share_top1 = .1)
teamsPredicted <- .teamsPredictedPower(data = select(player_predictions_DL_adj,-status),actualOrPred="predicted")



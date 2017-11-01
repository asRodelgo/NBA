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
load("data/modelNeuralnet5_PTS.Rdata")
#load("data/modelNeuralnet4_PTS.Rdata")
nn_Offense <- model$finalModel
load("data/modelNeuralnet5_PTSA.Rdata")
#load("data/modelNeuralnet4_PTSA.Rdata")
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
# Preparing the data
########################################
## Everything works based on individual players stats. I describe the way I think about this:

# 1. calculate playersNewPredicted ------------------------
# I take last season's rosters and calculate players projected stats for the new season. This is based on
# player similarity by age. I use euclidean distances from t-SNE dimensionality reduction by age. 
# The resulting file is: (The team variable (Tm) is according to last season. Some players will have multiple
playersNewPredicted <- read.csv("data/playersNewPredicted_Oct20.csv", stringsAsFactors = FALSE) %>% # from .computePredictedPlayerStats() in write_teams_predicted_stats_new_season.R
  distinct(Player, .keep_all=TRUE) %>% select(-c(Pos,Season,Age))
# make sure no shooting percentages are > 1
playersNewPredicted <- mutate_at(playersNewPredicted, vars(contains("Per")), function(x) ifelse(x >=1, quantile(x,.99), x))
# 2. Get current rosters -------------------------
# current_rosters contains players and teams as of Otober 20 2017, that is right at the start of the new season
current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE) %>% 
  distinct(Player, .keep_all=TRUE) # from .getLatestRosters(thisSeason="2017",previousSeason = FALSE)
# manually update Age for Jawun Evans (missing from basketballreference)
current_rosters <- mutate(current_rosters, Age = ifelse(grepl("Jawun",Player),20,Age))

# 3. Get rookies -------------------------
# Third piece of the puzzle are the new players, either from college or international
rookieEffStats <- read.csv("data/rookieEfficientStats.csv", stringsAsFactors = FALSE) %>% # write_Rookies_efficientStats in write_rookiesDraft.R
  select(-c(Season,Age)) %>% mutate(effFGPer = ifelse(effFGPer <= 0.001,FGPer,effFGPer)) # international players don't have this calculated so their per = 0
# 4. Put it all together ---------------------
# There are 6 different sets of players:
# a. Returning players whose names match in current_rosters and playersNewPredicted
# b. Returning players whose names don't match in current_rosters and playersNewPredicted
# c. Rookies whose names match in current_rosters and rookieEffStats
# d. Rookies whose names don't match in current_rosters and rookieEffStats (this set should be empty if rookieEffStats were calculated on or after Oct20)
# e. Players returning to the NBA and didn't play last season
# f. Rookies that didn't make the cut (this set should be empty if rookieEffStats were calculated on or after Oct20)

playerSet_a <- merge(playersNewPredicted,current_rosters, by = "Player") %>% distinct(Player,.keep_all=TRUE)
playerSet_c <- merge(rookieEffStats,current_rosters, by = "Player") %>% distinct(Player,.keep_all=TRUE)
playerSet_a_c <- merge(playerSet_a,playerSet_c, by = "Player") # this should be empty
playerSet_aPlusc <- bind_rows(playerSet_a,playerSet_c)  %>% distinct(Player,.keep_all=TRUE)
# players whose names didn't match come from merging playerSetaPlusc and current_rosters
playerSet_b <- merge(current_rosters, playerSet_aPlusc, by = "Player", all.x = TRUE)  %>% distinct(Player,.keep_all=TRUE) %>%
  filter(is.na(Tm.y)) %>% select(Player, Age.x)
# This is the list:
#[1] "Carrick Felix"      "Damien Wilkins"     "Darius Miller"      "Dwight Buycks"      "Ekpe Udoh"         
#[6] "Eric Moreland"      "Gary Payton II"     "Glenn Robinson III" "Jack Cooley"        "JaKarr Sampson"    
#[11] "Jeremy Evans"       "Julyan Stone"       "Kelly Oubre Jr."    "Larry Drew II"      "Lorenzo Brown"     
#[16] "Luis Montero"       "Mario Chalmers"     "Nate Wolters"       "Nene"               "Quincy Pondexter"  
#[21] "Shane Larkin"       "Sheldon Mac"        "Taurean Prince"     "Tim Hardaway"       "Vander Blue"

# This is the final list of unmatched players who actually played last season:
#c("Taurean Waller-Prince","Tim Hardaway 2","Nene Hilario","Glenn Robinson 2","Gary Payton 2","Kelly Oubre")

# I will change their names and update current_rosters and run sets a and c again:
current_rosters[which(current_rosters$Player == "Gary Payton II"),]$Player <- "Gary Payton 2"
current_rosters[which(current_rosters$Player == "Glenn Robinson III"),]$Player <- "Glenn Robinson 2"
current_rosters[which(current_rosters$Player == "Kelly Oubre Jr."),]$Player <- "Kelly Oubre"
current_rosters[which(current_rosters$Player == "Nene"),]$Player <- "Nene Hilario"
current_rosters[which(current_rosters$Player == "Taurean Prince"),]$Player <- "Taurean Waller-Prince"
current_rosters[which(current_rosters$Player == "Tim Hardaway"),]$Player <- "Tim Hardaway 2"
current_rosters[which(current_rosters$Player == "Sheldon Mac"),]$Player <- "Sheldon McClellan"
# run again sets a to c
playerSet_a <- merge(playersNewPredicted,current_rosters, by = "Player") %>% distinct(Player,.keep_all=TRUE)
playerSet_c <- merge(rookieEffStats,current_rosters, by = "Player") %>% distinct(Player,.keep_all=TRUE)
playerSet_a_c <- merge(playerSet_a,playerSet_c, by = "Player") # this should be empty
playerSet_aPlusc <- bind_rows(playerSet_a,playerSet_c)  %>% distinct(Player,.keep_all=TRUE) %>%
  select(Player,Pos,Season, Age,Tm=Tm.y,Exp,contains("Per"),contains("eff"))
# players whose names didn't match come from merging playerSetaPlusc and current_rosters
playerSet_b <- merge(current_rosters, playerSet_aPlusc, by = "Player", all.x = TRUE)  %>% distinct(Player,.keep_all=TRUE) %>%
  filter(is.na(Tm.y)) %>% select(Player, Pos = Pos.x, Age = Age.x, Tm = Tm.x, Exp = Exp.x)
# now this final set of non matched players correspond to those with a history in the NBA but didn't play in last season
# Next step is to calculate their predicted stats and add them to the final set
# NOTE: This takes approx 10 minutes to run
#playerSet_Leftover <- .computePredictedPlayerStats_Leftovers(playerSet_b) # from compute_PredictedLeftovers.R
#write.csv(playerSet_Leftover,"data/playerPredicted_Leftover.csv", row.names = FALSE)
playerSet_Leftover <- read.csv("data/playerPredicted_Leftover.csv", stringsAsFactors = FALSE)
playerSet_Leftover <- mutate(playerSet_Leftover, Season = current_rosters$Season[1]) %>%
  mutate_at(vars(contains("Per")), function(x) ifelse(x >= 1, mean(x, na.rm=TRUE), x)) # to avoid players with 100% shot accuracy (because they may have taken just very few shots and converted all)
# Now append together playerSetaPlusc and playerSet_Leftover for the final players stats predicted for new season
playersNewPredicted_Final <- bind_rows(playerSet_aPlusc,playerSet_Leftover)
# check this file has same rows as current_rosters
#checkFinalRosters <- merge(playersNewPredicted_Final,current_rosters, by="Player", all.x = TRUE)
# write file to avoid running this script again and again:
write.csv(playersNewPredicted_Final, "data/playersNewPredicted_Final_Oct20.csv",row.names = FALSE)
#
########################################
# Computing Powers
########################################
## This is the part where I take previously calculated predicted players stats and compute their Offense
## and Defense powers individually and as a team. This opens up a world of analysis and possibilities
#
# 1. load pre-calculated final players predictions -------------------
# effMin are not adjusted, i.e., rookies will have higher
# effMin than it would be expected. 
# NOTE: THIS LOCKS ROSTERS AS OF OCTOBER 20 2017. FURTHER CHANGES IN ROSTERS I WILL MAKE VIA MANUAL 
# TRANSFERS IF NEEDED
# Read the data
playersNewPredicted_Final <- read.csv("data/playersNewPredicted_Final_Oct20.csv",stringsAsFactors = FALSE)

# 2. adjust players minutes ---------------------------------------
# Reduce new players effMin (rookies, international, returning) by x%
# estimate empirically the % reduction in effMin
collegePlayersHist <- read.csv("data/collegePlayersHist.csv", stringsAsFactors = FALSE)
rookiesDraftHist <- read.csv("data/rookiesDraftHist.csv", stringsAsFactors = FALSE)
# compute % minutes played per player:
collegeMinutes <- merge(collegePlayersHist,rookiesDraftHist[,c("Player","Pick","Year")], by = "Player") %>%
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
# see if there is correlation between player Rank and effMin of play from college to NBA
plot(college2nbaMinutes$Rk,college2nbaMinutes$minDiff, xlim = c(0,100))
# no clear pattern so I will use the average for simplification. 
# Although Rank is different from Draft pick. Let's see this by draft pick for the top 30 picks:
plot(college2nbaMinutes$Pick,college2nbaMinutes$minDiff)
draftMinutesInNBA <- arrange(college2nbaMinutes, desc(Pick)) %>%
  group_by(Pick) %>%
  summarise(mean(minDiff)) %>%
  mutate(Pick = as.numeric(Pick)) %>%
  arrange(Pick)
barplot(draftMinutesInNBA$`mean(minDiff)`)
# Summary of effMin lost in % when moving from college to NBA (based on last 20 years of drafts):
# Pick #1: .065
# Pick #2: .016
# Picks #3-5: .02
# Picks #6-10: .25
# Picks #11-20: .34
# Picks #21-50: .4
# Picks #51-60: .45

# estimate difference in minutes played:
# add Pick round to rookie players:
rookiesDraft <- filter(rookiesDraftHist, Year >= as.numeric(thisYear)-2) # get last 3 drafts to include rookies like Ben Simmons who didn't play any minute last season
rookiesDraft[grepl("Frank Mason",rookiesDraft$Player),]$Player <- "Frank Mason III"
rookiesDraft[grepl("Dennis Smith",rookiesDraft$Player),]$Player <- "Dennis Smith Jr."
playersNewPredicted_Final <- merge(playersNewPredicted_Final, rookiesDraft[,c("Player","Pick")], by="Player",all.x=TRUE)
# For those not in the draft (international players or non-drafted players), average by the
# average percentage for the tail of the 2nd round picks (51-60)
col2nbaMinDiff <- mean(draftMinutesInNBA$`mean(minDiff)`[51:nrow(draftMinutesInNBA)]) 
playersNewPredicted_Final_adjMin <- mutate(playersNewPredicted_Final,Exp = ifelse(is.na(Exp),"1",Exp),
                                           Pick = ifelse(is.na(Pick),0,as.numeric(Pick))) %>% # in case Exp is NA for instance returning NBA players
  group_by(Player) %>%
  mutate(effMin = ifelse(Pick > 0 & Exp == "R",effMin*(1-draftMinutesInNBA$`mean(minDiff)`[Pick]),
                         ifelse(Exp == "R",effMin*(1-col2nbaMinDiff),effMin)))

# 3. adjust percent of play time -----------------------------------
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
playersNewPredicted_Final_adjMin2 <- .redistributeMinutes(playersNewPredicted_Final_adjMin, topHeavy = 7, topMinShare = .6, min_share_top1 = .105)
# The reason to go top_1 = 1.1 is to give more prominence to star players which adjust better when simulating
# wins in the regular season
# make sure Season column shows the new season to come and remove Pick column as i don't need it anymore
playersNewPredicted_Final_adjMin2 <- mutate(playersNewPredicted_Final_adjMin2, 
                                            Season = paste0(thisYear,"-",as.numeric(thisYear)+1)) %>%
  select(-Pick, -Exp) %>%
  as.data.frame()

# 4. compute team powers ---------------------------
# See teams_power.R for details. See if actual effMin matter (double check weighted means)
# playersNewPredicted_pumped <- mutate(playersNewPredicted_Final_adjMin2, effMin = ifelse(Tm == "UTA",effMin - .002,effMin))
# teamsPredicted_pumped <- .teamsPredictedPower(data = playersNewPredicted_pumped,actualOrPred="predicted")
# confrmed: effMin volume matters, I will transform in percentages
playersNewPredicted_Final_adjMinPer <- group_by(playersNewPredicted_Final_adjMin2, Tm) %>%
  mutate(effMin = effMin/sum(effMin,na.rm=TRUE)) %>%
  as.data.frame()
# Check percentage of minutes distribution matches the actuals from incrementShare
topMin <- 5
topX <- arrange(playersNewPredicted_Final_adjMinPer, desc(effMin)) %>%
  group_by(Tm) %>%
  top_n(topMin,effMin) %>%
  summarise(sum(effMin))
# there's some disparity that will affect the model. I'd rather have all teams's minutes
# equally distributed. ToDo: Fix effMin total = 30% for topHeavy in [8,13] range?

# Predict team powers
#effMinutes <- NULL # approx the average of all 
#teamPowers_newSeason <- merge(.computePower(playersNewPredicted_Final_adjMin2,"PTS","All",effMinutes,actualOrPredicted = "predicted"),.computePower(playersNewPredicted_Final_adjMin2,"PTSA","All",effMinutes,actualOrPredicted = "predicted"),by="team_season")
# Use 6-4-2 nnets (modelNeuralnet4_PTS.Rdata), maybe? Layers with higher number of neurons pick up more signal but also
# more noise. 
load("data/modelNeuralnet5_PTS.Rdata")
#load("data/modelNeuralnet4_PTS.Rdata")
nn_Offense <- model$finalModel
load("data/modelNeuralnet5_PTSA.Rdata")
#load("data/modelNeuralnet4_PTSA.Rdata")
nn_Defense <- model$finalModel

## Strip linearly relationed columns: FG, FGA, FG%,3P%,2P%,FT%,effFG%, effPTS
playersNewPredicted_Final_adjMinPer2 <- select(playersNewPredicted_Final_adjMinPer, -contains("Per"), -effFG, -effFGA, -effPTS, -effTRB)
## End of Strip

teamsPredicted <- .teamsPredictedPower(data = playersNewPredicted_Final_adjMinPer2,actualOrPred="predicted")
# make sure total PTS scored = total PTS against, although this won't change anything in win/loss predictions
# teamsPredicted <- mutate(teamsPredicted, TEAM_PTSAG = TEAM_PTSAG + (sum(TEAM_PTS)-sum(TEAM_PTSAG))/nrow(teamsPredicted))
# teamsPredicted <- mutate(teamsPredicted, basketAverage = TEAM_PTS - TEAM_PTSAG)

# 5. compute Offensive and Defensive powers for individual players -------------------
# The prediction works just as if each player was a team or if a team was composed of 18 copies of the same player
playersNewPredicted_OffDef <- mutate(playersNewPredicted_Final_adjMinPer2, Tm = Player, effMin = 1)
playersPredicted <- .teamsPredictedPower(data = playersNewPredicted_OffDef,actualOrPred="predicted")
playersPredicted <- mutate(playersPredicted, Player = substr(team_season,1,regexpr("_",team_season)-1),plusMinus = TEAM_PTS-TEAM_PTSAG) %>%
  select(Player,Offense = TEAM_PTS, Defense = TEAM_PTSAG, plusMinus) %>%
  as.data.frame()
# Add Experience to this data.frame. Rookie players or those with little experience will be statistically
# all over the place. Law of Big Numbers
playersPredicted2 <- merge(playersPredicted, playersNewPredicted_Final_adjMin[,c("Player","Exp","Age","Tm","effMin")], by = "Player") %>%
  mutate(adjPlusMinus = plusMinus*effMin*100) %>%
  group_by(Tm) %>%
  mutate(teamPlusMinus = sum(adjPlusMinus,na.rm=TRUE)) 

# 6. Simulate a few seasons using team estimated Offensive and Defensive powers
win_predictions <- simulate_n_seasons(10)
# Things to consider that may affect the results:
# rookies stat prediction transition from college/international
# the way I average players when not sufficient data on them
# make sure there are not outliers, or similar
# The way game scores are computed. Home/Away advantage. Factor in the distance between cities or conferences

# 7. Deal with changes in rosters (injuries, trades, etc) ---------------------
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



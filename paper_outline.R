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

# 1. calculate playersNewPredicted

playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE) # read historical players from write_playersHist.R
playersHist <- .rename_PlayerDuplicates(playersHist) # differentiate different players with the same name

playersNew <- playersHist %>% # keep only players last season
  filter(Season == max(as.character(Season))) %>%
  mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))

playersNewPredicted <- read.csv("data/playersNewPredicted.csv", stringsAsFactors = FALSE) # from .computePredictedPlayerStats() in write_teams_predicted_stats_new_season.R

# 2. Merge with current_rosters into playersNewPredicted_Current
playersNewPredicted_Current <- .mergePredictedWithCurrent() # from write_teams_predicted_stats_new_season.R

# 3. Complete playersNewPredicted with names not matching in current_rosters. Ex: Tim Hardaway vs. Tim Hardaway 2
current_rosters <- read.csv("data/currentRosters.csv", stringsAsFactors = FALSE) # .getLatestRosters from write_teams_predicted_stats_new_season.R
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






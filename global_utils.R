# Global utils -----------------------------
library(dplyr)
library(tsne)
library(rvest)
library(rlist) # save and load list objects

source("data/read_data.R")
source("helper_functions/teamStats.R")

global_mean <- mean(team_stats$PTS)
sigma <- 8 # constant std dev for all teams. ADJUST LATER ON!!

# Predicted team powers for the upcoming season -------------------------
teamsPredicted <- .teamsPredictedPower() # load teams powers (means for the Normal distributions)
# Season schedule for the upcoming season -------------------------------
seasonSchedule <- .seasonSchedule()



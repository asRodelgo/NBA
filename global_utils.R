# Global utils -----------------------------
library(dplyr)
library(tsne)
library(rvest)
library(rlist) # save and load list objects

source("data/read_data.R")
# Source all files from server_files directory and subdirectories
files <- list.files("helper_functions", full.names = TRUE, recursive = TRUE)
for (f in files) source(f, local = TRUE)

global_mean <- mean(team_stats$PTS)
sigma <- 8 # constant std dev for all teams. ADJUST LATER ON!!

# Predicted team powers for the upcoming season -------------------------
teamsPredicted <- .teamsPredictedPower() # load teams powers (means for the Normal distributions)
# Season schedule for the upcoming season -------------------------------
seasonSchedule <- .seasonSchedule()
# Reg Season Outcome for default predicted season
regSeasonOutcome <- .standings()



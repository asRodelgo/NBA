# Global utils -----------------------------
library(dplyr)
library(tsne)
library(rvest)

source("data/read_data.R")
source("helper_functions/teamStats.R")
global_mean <- mean(teams$TEAM_PTS)
sigma <- 10 # constant std dev for all teams

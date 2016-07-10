# Global utils -----------------------------
library(dplyr)
library(tsne)

source("data/read_data.R")
global_mean <- mean(teams$TEAM_PTS)
sigma <- 10 # constant std dev for all teams

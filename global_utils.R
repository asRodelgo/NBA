# Global utils -----------------------------
library(dplyr)
library(tsne)

global_mean <- mean(teams$TEAM_PTS)
sigma <- 10 # constant std dev for all teams

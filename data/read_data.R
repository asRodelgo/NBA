library(dplyr)

# Read data -------------------
players <- read.csv("data/nba_players.csv")
teams <- read.csv("data/nba_teams.csv")
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE)

#http://www.nbastuffer.com/2014-2015_NBA_Regular_Season_Player_Stats.html


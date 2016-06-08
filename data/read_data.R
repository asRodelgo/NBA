library(dplyr)

# Read data -------------------
players <- read.csv("data/nba_players.csv")
teams <- read.csv("data/nba_teams.csv")

global_mean <- mean(teams$PTS)
sigma <- 10 # constant std dev for all teams

team_home <- "CLE"
team_away <- "GSW"

teamH <- filter(teams, TeamCode == team_home)
teamA <- filter(teams, TeamCode == team_away)

muH <- teamH$PTS + 3 + teamA$PTSA - global_mean
muA <- teamA$PTS - 3 + teamH$PTSA - global_mean

pointsH <- round(rnorm(1,muH,sigma),0)
pointsA <- round(rnorm(1,muA,sigma),0)

while (poinsH-pointsA==0){
  
}
print(paste0(team_home,": ",round(rnorm(1,muH,sigma),0)," vs. ",
             team_away,": ",round(rnorm(1,muA,sigma),0)))



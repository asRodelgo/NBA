# Compute a model that uses players skills in a team as input and points scored by the team
# as output.
# Compute a second model that uses the same inputs and the team plus_minus as output
#
# My initial approach will be a neural network with #players as input units and 1 output unit
# or 2 output units if combining the 2 models above makes sense.
#
library(dplyr)
library(caret)
library(doMC) # use parallel processing on this machine through "foreach"
registerDoMC(2) # As far as I know my MAC works on 2 cores
library(neuralnet) # neural network for regression

# strip players from their names
players_names <- players$Player
players <- players[, -1]

# add team's average points in season (output variable y~ in the regression)
players <- merge(players, teams[,c("TeamCode","PTS")], by.x = "TEAM", by.y = "TeamCode")
players <- select(players, p)
  
# split data into train and test
teams_train <- head(teams,20)$TeamCode
teams_test <- filter(teams, !(TeamCode %in% teams_train))$TeamCode
training <- filter(players, TEAM %in% teams_train)
testing <- filter(players, TEAM %in% teams_test)

# scale the data for easier convergence of backpropagation algorithm
maxs <- apply(players[,-1], 2, max) 
mins <- apply(players, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))


sampleTeam <- filter(players, TEAM == "GSW")



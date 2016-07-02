# Compute a model that uses players skills in a team as input and points scored by the team
# as output.
# Compute a second model that uses the same inputs and the team plus_minus as output
#
# My initial approach will be a neural network with #players as input units and 1 output unit
# or 2 output units if combining the 2 models above makes sense.
#
# Reference: http://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
library(dplyr)
library(caret)
library(doMC) # use parallel processing on this machine through "foreach"
registerDoMC(2) # As far as I know my MAC works on 2 cores
library(neuralnet) # neural network for regression

# DATA PROCESSING --------------------------------------------

# Approach: Summarize variables at team level to obtain input vector for the model
# 1. By team: calculate weighted average of each characteristic: FGM, FGA, etc...
# 2. Weights correspond to total time on the court:
# w1 = MIN/48 ; w2 = GP/82
# w1*w2 ranges from 0 to 0.73. I add + 0.27 so players maximum usage approaches to 1
# Final weights: W = w1*w2 + 0.27

playersSumm <- players %>%
  group_by(TEAM) %>%
  mutate(Wt = (MIN/48)*(GP/82)+.27) %>%
  mutate_each(funs(weighted.mean(.,Wt)),-Player,-Wt) %>%
  distinct(W,L,MIN,.keep_all=TRUE)

playersSumm <- as.data.frame(playersSumm)
playersSumm <- playersSumm[, !(names(playersSumm) %in% c("Player","Wt"))]

# add team's average points in season (output variable y~ in the regression)
playersSumm <- merge(playersSumm, teams[,c("TeamCode","TEAM_PTS")], by.x = "TEAM", by.y = "TeamCode")

# scale the data for easier convergence of backpropagation algorithm
maxs <- apply(playersSumm[,-1], 2, max) 
mins <- apply(playersSumm[,-1], 2, min)

teamCodes <- playersSumm[,1]
scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
scaled <- cbind(teamCodes,scaled)

# CROSS VALIDATION --------------------------------------------

# k: number of splits train-test
# train_split: number of teams or percentage of data in training set
set.seed(450)
cv.error <- NULL
k <- 10
train_split <- 22
# neuralnet requires explicit formula for the model (f)
n <- names(training)
f <- as.formula(paste("TEAM_PTS ~", paste(n[!n %in% "TEAM_PTS"], collapse = " + ")))

for(i in 1:k){
  teams_train <- sample(teams$TeamCode,train_split)
  teams_test <- filter(teams, !(TeamCode %in% teams_train))$TeamCode
  training <- filter(scaled, teamCodes %in% teams_train)
  testing <- filter(scaled, teamCodes %in% teams_test)
  
  # remove non-numeric variables
  train_teamCodes <- training$teamCodes
  test_teamCodes <- testing$teamCodes
  training <- training[,-1]
  testing <- testing[,-1]
  
  ## Model Neural Network
  # Hidden layers and neurons per layer specified by hidden. 
  # Number of input neurons is the number of columns
  # Output neurons is 1 as we are doing regression (linear.output=T)
  # For classification problem, linear.output=F
  nn <- neuralnet(f,data=training,hidden=c(5,2),linear.output=T)
  
  # Prediction
  pr.nn <- compute(nn,testing[,-ncol(testing)])
  # Model results are scaled so need to scale them back to normal
  pr.nn_ <- pr.nn$net.result*(max(playersSumm$TEAM_PTS)-min(playersSumm$TEAM_PTS))+min(playersSumm$TEAM_PTS)
  test.r <- (testing$TEAM_PTS)*(max(playersSumm$TEAM_PTS)-min(playersSumm$TEAM_PTS))+min(playersSumm$TEAM_PTS)
  
  cv.error[i] <- sum((test.r - pr.nn_)^2)/nrow(testing)
  
}

# plot MSE distribution after C-V
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)




# ----------------------------------------------------------------
# Playground -----------------------------------------------------

# Individual model, no cross-validation: Plot the NNet and regression lines
# split data into train and test
teams_train <- head(teams,22)$TeamCode
teams_test <- filter(teams, !(TeamCode %in% teams_train))$TeamCode
training <- filter(scaled, teamCodes %in% teams_train)
testing <- filter(scaled, teamCodes %in% teams_test)

# remove non-numeric variables
train_teamCodes <- training$teamCodes
test_teamCodes <- testing$teamCodes
training <- training[,-1]
testing <- testing[,-1]

# Model Neural Network
set.seed(500)
library(neuralnet)
n <- names(training)
f <- as.formula(paste("TEAM_PTS ~", paste(n[!n %in% "TEAM_PTS"], collapse = " + ")))
# Hidden layers and neurons per layer specified by hidden. 
# Number of input neurons is the number of columns
# Output neurons is 1 as we are doing regression (linear.output=T)
# For classification problem, linear.output=F
nn <- neuralnet(f,data=training,hidden=c(5,2),linear.output=T)

# Plot
plot(nn)

# Prediction
pr.nn <- compute(nn,testing[,-ncol(testing)])
# Model results are scaled so need to scale them back to normal
pr.nn_ <- pr.nn$net.result*(max(playersSumm$TEAM_PTS)-min(playersSumm$TEAM_PTS))+min(playersSumm$TEAM_PTS)
test.r <- (testing$TEAM_PTS)*(max(playersSumm$TEAM_PTS)-min(playersSumm$TEAM_PTS))+min(playersSumm$TEAM_PTS)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(testing)

# Plot actual vs predicted
plot(test.r,pr.nn_,col='red',main='Actual vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')




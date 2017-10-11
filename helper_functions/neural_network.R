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
# Off_or_Def = "PTS" or "PTSA"
.prepareModel <- function(Off_or_Def, removeEffMin = TRUE){ 
  # Approach: Summarize variables at team level to obtain input vector for the model
  # 1. By team: calculate weighted average of each characteristic: FGM, FGA, etc...
  data_team <- .team_prepareAll() # If no arguments, will calculate for all teams for all seasons
  # 2. Weights correspond to percentage of total team time played, ie, sum(effMin) = 5
  # Probably more efficient to scale weights to add up to 1: Wt = effMin/5
  playersSumm <- data_team %>%
    filter(!(Tm == "TOT")) %>% # Those who played for more than 1 team have a Total team
    mutate(Wt = effMin/5) %>%
    group_by(Tm,Season) %>%
    summarise_if(is.numeric, funs(sum(.*Wt, na.rm=TRUE))) %>%
    dplyr::select(-Wt) %>%
    distinct(.keep_all=TRUE)
  
  # Paste Team and Season to have 1 field as identifier
  playersSumm <- playersSumm %>%
    mutate(team_season = paste0(Tm,"_",Season))
  playersSumm <- playersSumm[,!(names(playersSumm) %in% c("Tm","Season"))]
  
  if (removeEffMin) {
    playersSumm <- select(playersSumm, -effMin)
  }
  playersSumm <- as.data.frame(playersSumm)
  
  ## add team's average points in season (output variable y~ in the regression)
  # Paste Team and Season to have 1 field as identifier
  team_stats2 <- team_stats %>%
    mutate(team_season = paste0(teamCode,"_",Season))
  team_stats2 <- team_stats2[,!(names(team_stats2) %in% c("Team","teamCode","Season"))]
  # merge
  playersSumm <- merge(playersSumm, team_stats2[,c("team_season",Off_or_Def)], by = "team_season", all.x=TRUE)
  
  # No matter if I calculate Off or Def pts per season, I call this variable PTS for practical purposes
  names(playersSumm)[ncol(playersSumm)] <- "PTS" 
  
  return(playersSumm)
}

# For prediction before the prediction, i.e., no PTS per game data available
.prepareModelPrediction <- function(data = playersNew, thisTeam = "All", removeEffMin = TRUE){ 
  # Approach: Summarize variables at team level to obtain input vector for the model
  # 1. By team: calculate weighted average of each characteristic: FGM, FGA, etc...
  data_team <- .team_preparePredict(data, thisTeam) # If no arguments (or thisTeam = "All") will calculate for all teams
  # 2. Weights correspond to percentage of total team time played, ie, sum(effMin) = 5
  # Probably more efficient to scale weights to add up to 1: Wt = effMin/5
  if (!(thisTeam == "All")) data_team <- mutate(data_team, Tm = thisTeam)
    
  playersSumm <- data_team %>%
    filter(!(Tm == "TOT")) %>% # Those who played for more than 1 team have a Total team
    mutate(Wt = effMin/5) %>%
    group_by(Tm,Season) %>%
    summarise_if(is.numeric, funs(sum(.*Wt, na.rm=TRUE))) %>%
    dplyr::select(-Wt) %>%
    distinct(.keep_all=TRUE)
  
  # Paste Team and Season to have 1 field as identifier
  playersSumm <- playersSumm %>%
    mutate(team_season = paste0(Tm,"_",Season))
  playersSumm <- playersSumm[,!(names(playersSumm) %in% c("Tm","Season"))]
  
  if (removeEffMin) {
    playersSumm <- select(playersSumm, -effMin)
  }
  playersSumm <- as.data.frame(playersSumm)
  
#   ## add team's average points in season (output variable y~ in the regression)
#   # Paste Team and Season to have 1 field as identifier
#   team_stats2 <- team_statsNew %>%
#     mutate(team_season = paste0(teamCode,"_",Season))
#   team_stats2 <- team_stats2[,!(names(team_stats2) %in% c("Team","teamCode","Season"))]
  
  return(playersSumm)
}

# Once predicted, to be able to computePower, i.e., no PTS per game data available
.prepareModelOncePredicted <- function(data_team = playersNewPredicted, thisTeam = "All", removeEffMin = TRUE){ 
  # Approach: Summarize variables at team level to obtain input vector for the model
  # 1. By team: calculate weighted average of each characteristic: FGM, FGA, etc...
  # data_team <- .team_preparePredict(data, thisTeam) # If no arguments (or thisTeam = "All") will calculate for all teams
  # 2. Weights correspond to percentage of total team time played, ie, sum(effMin) = 5
  # Probably more efficient to scale weights to add up to 1: Wt = effMin/5
  if (!(thisTeam == "All")) data_team <- filter(data_team, Tm == thisTeam)
  
  playersSumm <- data_team %>%
    filter(!(Tm == "TOT")) %>% # Those who played for more than 1 team have a Total team
    mutate(Wt = effMin) %>%
    group_by(Tm,Season) %>%
    summarise_if(is.numeric, funs(sum(.*Wt, na.rm=TRUE))) %>%
    dplyr::select(-Wt) %>%
    distinct(.keep_all=TRUE)
  
  # Paste Team and Season to have 1 field as identifier
  playersSumm <- playersSumm %>%
    mutate(team_season = paste0(Tm,"_",Season))
  playersSumm <- playersSumm[,!(names(playersSumm) %in% c("Tm","Season"))]
  
  if (removeEffMin) {
    playersSumm <- select(playersSumm, -effMin)
  }
  playersSumm <- as.data.frame(playersSumm)
  
  #   ## add team's average points in season (output variable y~ in the regression)
  #   # Paste Team and Season to have 1 field as identifier
  #   team_stats2 <- team_statsNew %>%
  #     mutate(team_season = paste0(teamCode,"_",Season))
  #   team_stats2 <- team_stats2[,!(names(team_stats2) %in% c("Team","teamCode","Season"))]
  
  return(playersSumm)
}

# Max and Min for all variables in the available data. Used to rescale later on
.getScaleLimits <- function(Off_or_Def) {
  
  playersSumm <- .prepareModel(Off_or_Def)
  # scale the data for easier convergence of backpropagation algorithm
  maxs <- apply(playersSumm[,-1], 2, max) 
  mins <- apply(playersSumm[,-1], 2, min)
  scaleMaxMin <- data.frame(maxs,mins)
  
  return(scaleMaxMin)
}  

# Find the best parameters for the NNet using CV
.computeModel <- function(Off_or_Def) {

  playersSumm <- .prepareModel(Off_or_Def)
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  # scale the data [0,1] for easier convergence of backpropagation algorithm
  maxs <- scaleMaxMin$maxs 
  mins <- scaleMaxMin$mins
  
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  # CROSS VALIDATION --------------------------------------------
  # k: number of splits train-test
  # train_split: number of teams or percentage of data in training set
  set.seed(450)
  cv.error <- NULL
  cv_tr.error <- NULL
  k <- 10
  perc <- 0.80
  train_split <- round(perc*nrow(playersSumm))
  hidden_neurons <- c(6,4,2)
  #c(4,2)
  #c(6,4,2)
  # neuralnet requires explicit formula for the model (f)
  n <- names(scaled[,-1])
  f <- as.formula(paste("PTS ~", paste(n[!n %in% "PTS"], collapse = " + ")))
  
  for(i in 1:k){
    teams_train <- sample(playersSumm$team_season,train_split)
    teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
    training <- filter(scaled, team_season %in% teams_train)
    testing <- filter(scaled, team_season %in% teams_test)
    
    # remove non-numeric variables
    train_teamSeasonCodes <- training$team_season
    test_teamSeasonCodes <- testing$team_season
    training <- training[,-1]
    testing <- testing[,-1]
    
    ## Model Neural Network
    # Hidden layers and neurons per layer specified by hidden. 
    # Number of input neurons is the number of columns
    # Output neurons is 1 as we are doing regression (linear.output=T)
    # For classification problem, linear.output=F
    nn <- neuralnet(f,data=training,hidden=hidden_neurons,linear.output=T)
    
    # Prediction on testing dataset (out of sample)
    pr.nn <- compute(nn,testing[,-ncol(testing)])
    # Model results are scaled so need to scale them back to normal
    pr.nn_ <- pr.nn$net.result*(max(playersSumm$PTS)-min(playersSumm$PTS))+min(playersSumm$PTS)
    test.r <- (testing$PTS)*(max(playersSumm$PTS)-min(playersSumm$PTS))+min(playersSumm$PTS)
    # out of sample error
    cv.error[i] <- sum((test.r - pr.nn_)^2)/nrow(testing)
    
    # Prediction on training dataset (in sample)
    pr_tr.nn <- compute(nn,training[,-ncol(training)])
    pr_tr.nn_ <- pr_tr.nn$net.result*(max(playersSumm$PTS)-min(playersSumm$PTS))+min(playersSumm$PTS)
    train.r <- (training$PTS)*(max(playersSumm$PTS)-min(playersSumm$PTS))+min(playersSumm$PTS)
    # in sample error
    cv_tr.error[i] <- sum((train.r - pr_tr.nn_)^2)/nrow(training)
    
  }
  
  return(nn) # returns nnet model based on training data (perc of the total teams)
}

.computeModel_MxNet <- function(Off_or_Def){
  
  # Neural Network MXNET ---
  
  # mxnet package installation --
  #for mxnet package install in R using this command for only CPU**
  #cran <- getOption("repos")
  #cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
  #options(repos = cran)
  #install.packages("mxnet",dependencies = T)
  ### ---
  
  playersSumm <- .prepareModel(Off_or_Def)
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  # scale the data [0,1] for easier convergence of backpropagation algorithm
  maxs <- scaleMaxMin$maxs 
  mins <- scaleMaxMin$mins
  
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  ###
  set.seed(998)
  perc <- 0.75
  train_split <- round(perc*nrow(playersSumm))
  
  teams_train <- sample(playersSumm$team_season,train_split)
  teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
  training <- filter(scaled, team_season %in% teams_train)
  testing <- filter(scaled, team_season %in% teams_test)
  
  # remove non-numeric variables
  train_teamSeasonCodes <- training$team_season
  test_teamSeasonCodes <- testing$team_season
  training <- training[,-1]
  testing <- testing[,-1]
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)
  
  nnetGrid <-  expand.grid(layer1 = c(4,6,8), 
                           layer2 = c(2,3,4,5), 
                           layer3 = c(1,2,3),
                           learning.rate = c(0.07), 
                           momentum = 0.9, 
                           dropout = c(0.1), 
                           activation = c('relu','sigmoid','tanh')
  )
  
  library(mxnet)
  library(caret)
  #library(tidyverse)
  
  set.seed(825)
  # uses rmse for regression and softmax for classification by default (corresponds to parameter out_activation)
  nnetFit <- train(PTS ~ ., data = training, 
                   method = "mxnet", 
                   trControl = fitControl, 
                   tuneGrid = nnetGrid)
  
  ##########################################################################################
  # Model checking ----------------------------------------
  ##########################################################################################
  
  # check predictions
  model <- nnetFit
  #save(model, file = paste0("data/model_","nnetFit","_",Sys.Date(),".Rdata"))
  predict_data <- training
  predicted <- predict(model, newdata = predict_data)
  save(model, file = paste0("data/modelMxNet_",Off_or_Def,".Rdata"))
  predictions <- data.frame(actual_PTS = predict_data$PTS, predicted_PTS = predicted)
  plot(predictions)
}

.computeModel_neuralnet <- function(Off_or_Def){
  
  playersSumm <- .prepareModel(Off_or_Def)
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  # scale the data [0,1] for easier convergence of backpropagation algorithm
  maxs <- scaleMaxMin$maxs 
  mins <- scaleMaxMin$mins
  
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  ###
  set.seed(998)
  perc <- 0.75
  train_split <- round(perc*nrow(playersSumm))
  
  teams_train <- sample(playersSumm$team_season,train_split)
  teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
  training <- filter(scaled, team_season %in% teams_train)
  testing <- filter(scaled, team_season %in% teams_test)
  
  # remove non-numeric variables
  train_teamSeasonCodes <- training$team_season
  test_teamSeasonCodes <- testing$team_season
  training <- training[,-1]
  testing <- testing[,-1]
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)
  
  nnetGrid <-  expand.grid(layer1 = c(4,6,8), 
                           layer2 = c(2,3,4,5), 
                           layer3 = c(1,2,3)
  )
  
  library(neuralnet)
  library(caret)
  #library(tidyverse)
  
  set.seed(825)
  # uses rmse for regression and softmax for classification by default (corresponds to parameter out_activation)
  nnetFit <- train(PTS ~ ., data = training, 
                   method = "neuralnet", 
                   trControl = fitControl, 
                   tuneGrid = nnetGrid)
  
  ##########################################################################################
  # Model checking ----------------------------------------
  ##########################################################################################
  
  # check predictions
  model <- nnetFit
  #save(model, file = paste0("data/model_","nnetFit","_",Sys.Date(),".Rdata"))
  predict_data <- training
  predicted <- predict(model, newdata = predict_data)
  save(model, file = paste0("data/modelNeuralnet_",Off_or_Def,".Rdata"))
  predictions <- data.frame(actual_PTS = predict_data$PTS, predicted_PTS = predicted)
  plot(predictions)
}

# Once a model is selected, use this function to calculate team powers
.selectedModel <- function(Off_or_Def,removeEffMin = TRUE) {
  
  playersSumm <- .prepareModel(Off_or_Def, removeEffMin)
  # scale the data for easier convergence of backpropagation algorithm
  scaleMaxMin <- .getScaleLimits(Off_or_Def)
  maxs <- scaleMaxMin$maxs 
  mins <- scaleMaxMin$mins
  
  team_season <- playersSumm[,1]
  scaled <- as.data.frame(scale(playersSumm[,-1], center = mins, scale = maxs - mins))
  scaled <- cbind(team_season,scaled)
  
  # train_split: number of teams or percentage of data in training set
  set.seed(450)
  perc <- 0.80
  train_split <- round(perc*nrow(playersSumm))
  hidden_neurons <- c(6,4,2)
  #c(4,2)
  #c(6,4,2)
  # neuralnet requires explicit formula for the model (f)
  n <- names(scaled[,-1])
  f <- as.formula(paste("PTS ~", paste(n[!n %in% "PTS"], collapse = " + ")))
  
  teams_train <- sample(playersSumm$team_season,train_split)
  #teams_test <- filter(playersSumm, !(team_season %in% teams_train))$team_season
  training <- filter(scaled, team_season %in% teams_train)
  #testing <- filter(scaled, team_season %in% teams_test)
  
  # remove non-numeric variables
  train_teamSeasonCodes <- training$team_season
  #test_teamSeasonCodes <- testing$team_season
  training <- training[,-1]
  #testing <- testing[,-1]
  
  ## Model Neural Network
  # Hidden layers and neurons per layer specified by hidden. 
  # Number of input neurons is the number of columns
  # Output neurons is 1 as we are doing regression (linear.output=T)
  # For classification problem, linear.output=F
  nn <- neuralnet(f,data=training,hidden=hidden_neurons,linear.output=T)

  return(nn) # returns nnet model based on training data (perc of the total teams)
}

# Plotting --------------------------------
# # plot MSE distribution after C-V
# boxplot(cv.error,xlab='MSE CV',col='cyan',
#         border='blue',names='CV error (MSE)',
#         main='CV error (MSE) for NN',horizontal=TRUE)
# 
# plot(pr.nn_, test.r)
# plot(pr_tr.nn_, train.r)

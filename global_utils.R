# Global utils -----------------------------
library(tidyverse)
library(tsne)
library(rvest)
library(rlist) # save and load list objects
library(scales)
library(RColorBrewer)
# radar charts in ggplot2
#library(devtools)
#devtools:install_github("jerryzhujian9/ezmisc")
#library(ezmisc)#

thisYear <- substr(Sys.Date(),1,4)
if (substr(Sys.Date(),6,7) > '08'){
  seasonOffset <- 0
} else {
  seasonOffset <- 1
}
thisSeason <- paste0(as.numeric(substr(Sys.Date(),1,4))-seasonOffset,"-",as.numeric(substr(Sys.Date(),1,4))-seasonOffset+1) 
# Source all files from server_files directory and subdirectories
files <- list.files("helper_functions", full.names = TRUE, recursive = TRUE)
for (f in files) source(f, local = TRUE)

# load the data
source("data/read_data.R")

global_mean <- mean(team_stats$PTS)
sigma <- 8 # constant std dev for all teams. ADJUST LATER ON!!

# load teams powers (means for the Normal distributions)
# Season schedule for the upcoming season -------------------------------

#seasonSchedule <- .seasonSchedule()

# Precompute Reg Season results for all season
#teamsPredicted <- .teamsPredictedPower()
regSeasonOutcome <- .standings(real=TRUE)
playoffs <- .getPlayoffResults()

# tSNE
data_tsne <- .tSNE_prepare_All() # for tSNE visualization from similarityFunctions.R
data_tsne_sample <- filter(data_tsne,Season > "1995-1996")
# tsne_points are pre-calculated from write_tSNE_All.R and saved in data/ directory
# using this function: tsne_points <- write_tSNE_compute_All()
if (!nrow(data_tsne_sample)==nrow(tsne_points)){ # in case labels and coordinates have different sizes
  tsne_ready <- tsne_points
} else {
  tsne_ready <- cbind(data_tsne_sample,tsne_points)
}

names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
names(tsne_ready)[ncol(tsne_ready)] <- "y"
# Default selector choices for tsne -----------
teams_list <- sort(unique(data_tsne_sample$Tm))
ages_list <- sort(unique(data_tsne_sample$Age))
seasons_list <- sort(unique(data_tsne_sample$Season))
players_list <- sort(unique(data_tsne_sample$Player))
skills_list <- names(data_tsne_sample)[6:ncol(data_tsne_sample)]
#
tsne_url <- "http://distill.pub/2016/misread-tsne/"
#write.csv(tsne_ready, "data/tsne_ready.csv",row.names = FALSE)


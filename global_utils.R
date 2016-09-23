# Global utils -----------------------------
library(dplyr)
library(tidyr)
library(tsne)
library(rvest)
library(rlist) # save and load list objects
library(scales)
library(RColorBrewer)
# ggplot2 radar charts
#devtools::install_github("ricardo-bion/ggradar", 
#                         dependencies=TRUE)
library(ggradar)

source("data/read_data.R")
# Source all files from server_files directory and subdirectories
files <- list.files("helper_functions", full.names = TRUE, recursive = TRUE)
for (f in files) source(f, local = TRUE)

global_mean <- mean(team_stats$PTS)
sigma <- 8 # constant std dev for all teams. ADJUST LATER ON!!

# load teams powers (means for the Normal distributions)
# Season schedule for the upcoming season -------------------------------

#seasonSchedule <- .seasonSchedule()

# Reg Season Outcome for default predicted season
regSeasonOutcome <- .standings(real=TRUE)
playoffs <- .getPlayoffResults()

# tSNE
data_tsne <- .tSNE_prepare_All()
data_tsne_sample <- filter(data_tsne,Season > "1990-1991")
tsne_ready <- cbind(data_tsne_sample,tsne_points)
names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
names(tsne_ready)[ncol(tsne_ready)] <- "y"
# Default selector choices for tsne -----------
teams_list <- sort(unique(data_tsne_sample$Tm))
ages_list <- sort(unique(data_tsne_sample$Age))
seasons_list <- sort(unique(data_tsne_sample$Season))
players_list <- sort(unique(data_tsne_sample$Player))
skills_list <- names(data_tsne_sample)[6:ncol(data_tsne_sample)]
#
#write.csv(tsne_ready, "data/tsne_ready.csv",row.names = FALSE)


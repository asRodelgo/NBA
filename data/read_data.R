# Read data -----------------------------------------------------

# Read players stats -------------------------------------------

####### END OF PREVIOUS SEASON: These data is ready as soon as the NBA season ends #####
# and remove duplicate names (due to father-son, or others)
playersHist <- read.csv("data/playersHist.csv", stringsAsFactors = FALSE) # from write_playersHist.R
playersHist <- .rename_PlayerDuplicates(playersHist) # differentiate different players with the same name
# Read team stats for all seasons ------------------------------
team_stats <- read.csv("data/teamStats.csv") # from write_TeamStats.R

## These data may not change from season to season
franchises <- read.csv("data/franchisesHistory.csv",stringsAsFactors = FALSE) # Manually obtained. No need to change unless there are changes in team names or new teams get added to the league
team_stats <- merge(team_stats,franchises,by.x="Team",by.y="Franchise",all.x=TRUE)
# conferences according to last season
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE) # Same as franchises 

# Read pre-calculated tSNE coordinates per Age -----------------
# from write_tsneBlocks.R
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- read.csv(paste0("data/tsneBlock","_",a,".csv"))
}

#### NEW SEASON/CURRENT SEASON --------------------------------- 
# Actual Season schedule
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE) # from write_seasonSchedule.R
datesRange <- unique(realSeasonSchedule$Date)

# If not new data yet (transfers not finished so teams rosters not final), -----------------------------------------
dataNewSeason <- FALSE
if (dataNewSeason==FALSE){
  # use last season's as new data, removing PTS & PTSA
  team_statsNew <- team_stats %>%
    filter(Season == max(as.character(Season))) %>%
    mutate(W = 0, L = 0, PTS = 0, PTSA = 0, SRS = 0, 
           Season = paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)) %>%
    distinct(Team, .keep_all=TRUE)
  # same for players
  playersNew <- playersHist %>%
    filter(Season == max(as.character(Season))) %>%
    mutate(Season = as.factor(paste0(as.numeric(substr(Season,1,4))+1,"-",as.numeric(substr(Season,1,4))+2)))
}

# Read pre-calculated nnetwork models -------------------------
nn_Offense <- list.load("data/nn_Offense.rds")
nn_Defense <- list.load("data/nn_Defense.rds")

# Predicted team powers for the upcoming season -------------------------
# Default to pre-calculated for quick start of the app
teamsPredicted <- read.csv("data/teamsPredicted.csv", 
                           colClasses = c("factor",rep("numeric",2),rep("character",2)))

#.teamsPredictedPower() 
tsne_points <- read.csv("data/tsne_points_All.csv",stringsAsFactors = FALSE)

####################################################################################
#### CONDITIONAL runs ####

# ## When Playoff comes
# # Adjust minutes played by star players (top 5 in minutes by .2 percent )
# # (for instance come playoffs time)
# playersNew <- .adjust_Minutes(playersNew,.2)
# 
# ## Trade Players
# playersNew <- .trade_Players(playersNew,"Jimmy Butler","CHI","Zach LaVine","MIN")
# playersNew <- .trade_Players(playersNew,"Paul George","IND","Edy Tavares","CLE")
# playersNew <- filter(playersNew, !(Player=="Edy Tavares"))

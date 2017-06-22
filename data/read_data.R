# Read data -----------------------------------------------------

# Read players stats -------------------------------------------
#players <- read.csv("data/nba_players.csv") # from nba.com
# TASK: Update this. I grabbed the data manually from nba.com. See if doable from
# breference. Write a writer.
playersHist <- read.csv("data/nba_players_allSeasons.csv", stringsAsFactors = FALSE)
##
##
playersHist <- mutate(playersHist, Player = gsub("*","",Player, fixed=TRUE)) %>%
  group_by(Player) %>%
  mutate(yearBorn = as.numeric(substr(Season,1,4)) - Age) %>%
  as.data.frame()
# Players with the same name will create silly duplicates. Identify them
playerDups <- group_by(playersHist,Player) %>%
  filter(max(yearBorn)-min(yearBorn) > 1) %>%
  distinct(Player, Season, .keep_all = TRUE) %>%
  arrange(Player,desc(Season)) %>%
  group_by(Player,yearBorn) %>%
  arrange(Player, yearBorn) %>%
  distinct(Player,yearBorn,.keep_all=TRUE) %>%
  dplyr::select(Player,yearBorn) %>%
  group_by(Player) %>%
  mutate(id = row_number()) %>%
  as.data.frame()
# Rename them: second: 2, third: 3, etc.
playersHist <- merge(playersHist,playerDups, by=c("Player","yearBorn"),all.x = TRUE) %>%
  mutate(Player = ifelse(!is.na(id), ifelse(id > 1, paste(Player,id),Player), Player)) %>%
  as.data.frame()

# Read pre-calculated tSNE coordinates per Age -----------------
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- read.csv(paste0("data/tsneBlock","_",a,".csv"))
}

# Read team stats for all seasons ------------------------------
#teams <- read.csv("data/nba_teams.csv") # from nba.com
team_stats <- read.csv("data/teamStats.csv")
franchises <- read.csv("data/franchisesHistory.csv",stringsAsFactors = FALSE)
team_stats <- merge(team_stats,franchises,by.x="Team",by.y="Franchise",all.x=TRUE)
# conferences according to last season
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE)

#### NEW SEASON/CURRENT SEASON --------------------------------- 

# Actual Season schedule
realSeasonSchedule <- read.csv("data/realSeasonSchedule.csv",stringsAsFactors = FALSE)
datesRange <- unique(realSeasonSchedule$Date)
# If not new data yet (transfers not finished so teams rosters not final), -----------------------------------------
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

# Read pre-calculated nnetwork models -------------------------
nn_Offense <- list.load("data/nn_Offense.rds")
nn_Defense <- list.load("data/nn_Defense.rds")

# Predicted team powers for the upcoming season -------------------------
# Default to pre-calculated for quick start of the app
teamsPredicted <- read.csv("data/teamsPredicted.csv", 
                           colClasses = c("factor",rep("numeric",2),rep("character",2)))

###### ---------------------------------------------------

#.teamsPredictedPower() 
tsne_points <- read.csv("data/tsne_points_All.csv",stringsAsFactors = FALSE)


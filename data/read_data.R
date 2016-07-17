# Read data -----------------------------------------------------
players <- read.csv("data/nba_players.csv")
playersHist <- read.csv("data/nba_players_allSeasons.csv")
teams <- read.csv("data/nba_teams.csv")
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE)

# Read pre-calculated tSNE coordinates per Age -----------------
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- read.csv(paste0("data/tsneBlock","_",a,".csv"))
}

# Read team stats for all seasons ------------------------------
team_stats <- read.csv("data/teamStats.csv")
franchises <- read.csv("data/franchisesHistory.csv")
team_stats <- merge(team_stats,franchises,by.x="Team",by.y="Franchise",all.x=TRUE)

# If not new data yet, -----------------------------------------
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




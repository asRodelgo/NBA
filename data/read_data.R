# Read data -------------------
players <- read.csv("data/nba_players.csv")
playersHist <- read.csv("data/nba_players_allSeasons.csv")
teams <- read.csv("data/nba_teams.csv")
conferences <- read.csv("data/nba_conferences.csv", stringsAsFactors = FALSE)
#http://www.nbastuffer.com/2014-2015_NBA_Regular_Season_Player_Stats.html
# Read pre-calculated tSNE coordinates per Age
tsneBlock <- list()
for (a in 18:41){
  tsneBlock[[a]] <- read.csv(paste0("data/tsneBlock","_",a,".csv"))
}
# Read team stats for all seasons
team_stats <- read.csv("data/teamStats.csv")


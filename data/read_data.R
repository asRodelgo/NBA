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


# Read teams stats for all seasons
url <- "http://www.basketball-reference.com/leagues/NBA_2015.html"
team_stats <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="all_standings"]/table') %>%
  html_table(fill = TRUE)
team_stats <- team_stats[[1]]
team_stats <- team_stats[2:nrow(team_stats),1:8]
names(team_stats) <- c("Team",team_stats[1,2:ncol(team_stats)])
team_stats <- team_stats[-1,]
team_stats <- team_stats[!(team_stats$W=="W"),]
team_stats <- team_stats[!grepl("division",tolower(team_stats[,1])),]
team_stats <- select(team_stats, -`W/L%`, -GB)
team_stats <- mutate_each(team_stats, funs(as.numeric), -Team)
trim <- gsub("\\*?\\([0-9]+\\)","",team_stats$Team)
trim <- gsub("*","",trim,fixed = TRUE)
blank <- trim[1][nchar(trim[1])]
trim <- gsub(NA,"",trim)


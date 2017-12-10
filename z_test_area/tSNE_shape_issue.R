##### -------------------------------------------------
# testing shape changes in tSNE with any new data point
##### -------------------------------------------------
#
library(tidyverse)
library(tsne)
data <- read.csv("data/playersNewPredicted_Final_adjMin.csv", stringsAsFactors = FALSE)
# variables set up
thisYear <- substr(Sys.Date(),1,4)
if (substr(Sys.Date(),6,7) > '08'){
  seasonOffset <- 0
} else {
  seasonOffset <- 1
}
thisSeason <- paste0(as.numeric(substr(Sys.Date(),1,4))-seasonOffset,"-",as.numeric(substr(Sys.Date(),1,4))-seasonOffset+1) 
# helper functions
files <- list.files("helper_functions", full.names = TRUE, recursive = TRUE)
for (f in files) source(f, local = TRUE)
# -------------------------------------------------
# For instance: Replace any player with average player
anyPlayer <- "Bradley Beal"
otherPlayer <- "Ricky Rubio"
avg_Exp <- mean(as.numeric(gsub("R","0",data$Exp)), na.rm=TRUE)
averagePlayer <- .calculate_AvgPlayer(data) %>% mutate(Exp = as.character(floor(avg_Exp))) %>%
  select(Player, Pos, Season, Age, Tm, Exp, everything()) %>%
  mutate(Age = round(Age,0)) %>%
  as.data.frame()
averagePlayer$Tm <- filter(data, Player == anyPlayer)$Tm
data2 <- bind_rows(data, averagePlayer)
# update players and teams resulting from trade
data2 <- .redistributeMinutes(data = data2, topHeavy = 7, topMinShare = .6, min_share_top1 = .105)

# ------------------------------------------------- 
# data contains original data, data2 modified data. Run t-SNE on both and see differences:
# 1. tSNE original data
tsne_data <- .compute_tSNE_ready_players(data)
plot(tsne_data$x,tsne_data$y)
text(x = filter(tsne_data, Player == anyPlayer)$x, y = filter(tsne_data, Player == anyPlayer)$y,
     labels = "X", col = "blue")
text(x = filter(tsne_data, Player == otherPlayer)$x, y = filter(tsne_data, Player == otherPlayer)$y,
     labels = "Y", col = "blue")
# 2. tSNE modified data
tsne_data2 <- .compute_tSNE_ready_players(data2)
plot(tsne_data2$y,tsne_data2$x)
text(x = filter(tsne_data2, Player == anyPlayer)$y, y = filter(tsne_data2, Player == anyPlayer)$x,
     labels = "X", col = "red")
text(x = filter(tsne_data2, Player == otherPlayer)$x, y = filter(tsne_data2, Player == otherPlayer)$y,
     labels = "Y", col = "red")




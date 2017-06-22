## Update the historical players database: playersHist

# Look in basketballreference.com and loop for all seasons
# Example: http://www.basketball-reference.com/leagues/NBA_2017_per_game.html
write.csv(playersHist, "data/playersHist.csv",row.names = FALSE)
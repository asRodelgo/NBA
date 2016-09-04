# Write default team powers
teamsPredicted <- .teamsPredictedPower() # load teams powers (means for the Normal distributions)
write.csv(teamsPredicted, "data/teamsPredicted.csv",row.names = FALSE)

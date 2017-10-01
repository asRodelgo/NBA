##### Paper Outline -------------------------------
# This script will describe step by step data preparation needed to get intended outcomes
# Date: October 1, 2017
##### ---------------------------------------------
## Step 1: Take rosters at the end of last season. They will contain regular season stats. 
# Use this as the baseline to predict their stats for the upcoming season. 
# Then take all the rookies from the rosters as of today (trades may still happen before the regular season
# gets started but those can be dealt with using tradePlayers functions). With these rookies, calculate
# their stats from their college years (last 3 seasons) or European league stats (all available seasons). 
# Leftovers from these two categories are averaged out.

# 1. calculate playersNewPredicted
playersNewPredicted <- .computePredictedPlayerStats
# 2. Merge with current_rosters into playersNewPredicted_Current
playersNewPredicted_Current <- .mergePredictedWithCurrent()
# 3. Complete playersNewPredicted with names not matching in current_rosters. Ex: Tim Hardaway vs. Tim Hardaway 2
playersMatch <- merge(current_rosters,playersNewPredicted, by = "Player", all.x = TRUE) %>%
  distinct(Player)

playersNonMatch <- filter(playersNewPredicted_Current, !(Player %in% playersMatch$Player))

playersManuallyChanged <- filter(playersNonMatch, Player %in% c(
  "Mike Dunleavy 2","Taurean Waller-Prince","Tim Hardaway 2","Nene Hilario","Glenn Robinson 2",
  "Gary Payton 2","Gerald Henderson 2","Kelly Oubre"
))

playersManuallyChanged <- filter(playersManuallyChanged, !(grepl("Dunleavy",Player) & !(Tm =="ATL") )) # now remove those with several teams:
playersNewPredicted_Current <- filter(playersNewPredicted_Current, !is.na(Exp)) # now remove those non-matching from playersNewPredicted
playersNewPredicted_Current <- bind_rows(playersNewPredicted_Current,playersManuallyChanged) # and add the manually changed to match non-matching players
playersNewPredicted_Current <- select(playersNewPredicted_Current, -c(Exp,College))






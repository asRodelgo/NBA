# Compute season fixtures ---------------
# Rules:
# Each team plays 82 regular season games
# Of those, 52 against 14 same conference teams
# Will play 10 of those teams 4 times (2 home + 2 away)
# Will play remaining 4, 3 times (randomize between: 2 at home or 1 at home)
# Of the 30 games against non-conference teams, will play 2 games each, 1 home, 1 away.
# ---------------------------------------

set.seed(111)
teamsReordered <- sample(conferences$TeamCode)

schedule <- matrix(nrow=30,ncol=30,dimnames = list(teamsReordered,teamsReordered))

for (i in 1:29){
  count3games <- length(schedule[which(schedule[i,]+schedule[,i]==3)]) # each team plays 3 games against 4 of their fellow conference teams, 4 against the rest
  limit_home <- 0 # Out of those 3 games, 1 has to be away twice, and 2 away twice
  for (j in (i+1):30){
    thisTeam <- dimnames(schedule)[[1]][i]
    vsTeam <- dimnames(schedule)[[2]][j]
    thisConf <- filter(conferences, TeamCode == thisTeam)$Conference
    vsConf <- filter(conferences, TeamCode == vsTeam)$Conference
    thisConfTeams <- filter(conferences, Conference == thisConf)$TeamCode
    if (!(thisConf == vsConf)){
      schedule[i,j] <- 1
      schedule[j,i] <- 1
    } else if (count3games < 4) {
      if (limit_home < 2) {
        schedule[i,j] <- 2
        schedule[j,i] <- 1
        limit_home <- limit_home + 1
      } else {
        schedule[i,j] <- 1
        schedule[j,i] <- 2
      }
      count3games <- count3games + 1
    } else {
      schedule[i,j] <- 2
      schedule[j,i] <- 2
    }
  }
}

# set up the schedule for the regular season. On average there are 6 games per day
gamesUrn <- data.frame()

urn_count <- 1
for (i in 1:30){
  for (j in 1:30){
    if (!(i==j)){
      if (schedule[i,j]==2){
        gamesUrn[urn_count,1] <- i
        gamesUrn[urn_count,2] <- j
        urn_count <- urn_count + 1
        gamesUrn[urn_count,1] <- i
        gamesUrn[urn_count,2] <- j
        urn_count <- urn_count + 1
      } else {
        gamesUrn[urn_count,1] <- i
        gamesUrn[urn_count,2] <- j
        urn_count <- urn_count + 1
      }
    }
  }
}

season <- data.frame()
set.seed(23)
gamesUrnSamp <- sample_frac(gamesUrn)

day <- 1
day_count <- 0
for (i in 1:nrow(gamesUrnSamp)){
  if (day_count>6){
    day_count <- 0
    day <- day + 1
  }  

  season[i,1] <- day
  season[i,2] <- dimnames(schedule)[[1]][gamesUrnSamp[i,1]]
  season[i,3] <- dimnames(schedule)[[1]][gamesUrnSamp[i,2]]
  
  day_count <- day_count + 1
  
}
# Next: avoid same team to play in a given day


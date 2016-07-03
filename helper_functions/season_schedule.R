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
  count3games <- length(schedule[which(schedule[i,]==3)]) # each team plays 3 games against 4 of their fellow conference teams, 4 against the rest
  for (j in (i+1):30){
    thisTeam <- dimnames(schedule)[[1]][i]
    vsTeam <- dimnames(schedule)[[2]][j]
    thisConf <- filter(conferences, TeamCode == thisTeam)$Conference
    vsConf <- filter(conferences, TeamCode == vsTeam)$Conference
    thisConfTeams <- filter(conferences, Conference == thisConf)$TeamCode
    if (!(thisConf == vsConf)){
      schedule[i,j] <- 2
      schedule[j,i] <- 2
    } else if (count3games<4) {
        schedule[i,j] <- 3
        schedule[j,i] <- 3
        count3games <- count3games + 1
    } else {
      schedule[i,j] <- 4
      schedule[j,i] <- 4
    }
  }
}

# set up the schedule for the regular season. On average there are 6 games per day


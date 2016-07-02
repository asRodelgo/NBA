global_mean <- mean(teams$PTS)
sigma <- 10 # constant std dev for all teams

# Single game simulation ----------------
team_home <- "CLE"
team_away <- "GSW"

teamH <- filter(teams, TeamCode == team_home)
teamA <- filter(teams, TeamCode == team_away)

muH <- teamH$PTS + 3 + teamA$PTSA - global_mean
muA <- teamA$PTS - 3 + teamH$PTSA - global_mean

pointsH <- round(rnorm(1,muH,sigma),0)
pointsA <- round(rnorm(1,muA,sigma),0)

while (pointsH-pointsA==0){ # overtime tie-breaker
  extraH <- round(rnorm(1,muH*5/48,sigma/3),0)
  extraA <- round(rnorm(1,muA*5/48,sigma/3),0)
  pointsH <- pointsH + extraH
  pointsA <- pointsA + extraA
}
print(paste0(team_home,": ",pointsH," vs. ",
             team_away,": ",pointsA))

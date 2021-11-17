library (StatsBombR)

Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
Matches = Matches %>% filter(competition.competition_name=="UEFA Euro")
data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)
data_boxscore = data_boxscore[data_boxscore$teamLoc == "Home",]
endMatch = max(which(data_boxscore$gmDate == "2018-04-11"))
startMatch = min(which(data_boxscore$gmDate == "2018-03-11"))
data_B = as.data.frame(matrix(NA, (endMatch - startMatch + 1), 32))
colnames(data_B) = c("gmDate", "HomeTeam(A)", "AwayTeam(B)", "SameConf", "HistData", 
                    "confname_A", "homerate_A", "awayrate_A", "last10_A", "homescorewin_A", "homescorelost_A", "awayscorewin_A", "awayscorelost_A",  "confrate_A", "numberdayoff_A", "lastgame_A", "totalmatch_A", "confmatch_A",
                    "confname_B", "homerate_B", "awayrate_B", "last10_B", "homescorewin_B", "homescorelost_B", "awayscorewin_B", "awayscorelost_B",  "confrate_B", "numberdayoff_B", "lastgame_B", "totalmatch_B", "confmatch_B", "result")
data_B[, 1] = data_boxscore$gmDate[startMatch:endMatch]
data_B[, 2] = data_boxscore$teamAbbr[startMatch:endMatch]
data_B[, 3] = data_boxscore$opptAbbr[startMatch:endMatch]
data_B[, 4] = (data_boxscore$teamConf == data_boxscore$opptConf)[startMatch:endMatch]
for (i in 1:nrow(data_B)) {
  data_B[i, 5] = data_hist$hist_rate[which(data_hist$team1 == as.character(data_B[i,2]) & data_hist$team2 == as.character(data_B[i,3]))]
}
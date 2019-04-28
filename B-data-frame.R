data_boxscore = data_boxscore[data_boxscore$teamLoc == "Home",]
endMatch = max(which(data_boxscore$gmDate == "2018-04-11"))
startMatch = min(which(data_boxscore$gmDate == "2018-03-11"))
data_B = as.data.frame(matrix(NA, (endMatch - startMatch + 1), 56))
colnames(data_B) = c("gmDate", "HomeTeam(A)", "AwayTeam(B)", "SameConf", "HistData", 
                    "confname_A", "homerate_A", "awayrate_A", "last10_A", "homescorewin_A", "homescorelost_A", "awayscorewin_A", "awayscorelost_A",  "confrate_A", "numberdayoff_A", "lastgame_A", "totalmatch_A", "confmatch_A",
                    "homematch_A", "awaymatch_A", "l1_A", "l2_A", "l3_A", "l4_A", "l5_A", "l6_A", "l7_A", "l8_A", "l9_A", "l10_A", 
                    "confname_B", "homerate_B", "awayrate_B", "last10_B", "homescorewin_B", "homescorelost_B", "awayscorewin_B", "awayscorelost_B",  "confrate_B", "numberdayoff_B", "lastgame_B", "totalmatch_B", "confmatch_B", 
                    "homematch_B", "awaymatch_B", "l1_B", "l2_B", "l3_B", "l4_B", "l5_B", "l6_B", "l7_B", "l8_B", "l9_B", "l10_B","result")
data_B[, 1] = data_boxscore$gmDate[startMatch:endMatch]
data_B[, 2] = data_boxscore$teamAbbr[startMatch:endMatch]
data_B[, 3] = data_boxscore$opptAbbr[startMatch:endMatch]
data_B[, 4] = (data_boxscore$teamConf == data_boxscore$opptConf)[startMatch:endMatch]
for (i in 1:nrow(data_B)) {
  data_B[i, 5] = data_hist$hist_rate[which(data_hist$team1 == as.character(data_B[i,2]) & data_hist$team2 == as.character(data_B[i,3]))]
}
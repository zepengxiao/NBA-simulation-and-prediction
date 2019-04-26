data_hist = matrix(NA, 900, 5)
data_hist = as.data.frame(data_hist)
colnames(data_hist) = c("team1", "team2", "hist_win", "hist_lost", "hist_rate")

for (i in 1:30) {
  for (j in 1:30) {
    if (i != j) {
      data_hist[(i-1)*30 + j, 1] = as.character(team_level[i])
      data_hist[(i-1)*30 + j, 2] = as.character(team_level[j])
      totalResult1 = data_boxscore_total$teamRslt[which((data_boxscore_total$teamAbbr == team_level[i]) & (data_boxscore_total$opptAbbr == team_level[j]))]
      totalResult2 = data_boxscore_total$teamRslt[which((data_boxscore_total$teamAbbr == team_level[j]) & (data_boxscore_total$opptAbbr == team_level[i]))]
      totalGame = length(totalResult1) + length(totalResult2)
      totalWin = sum(as.numeric(totalResult1) - 1) + (length(totalResult2) - sum(as.numeric(totalResult2) - 1))
      totalLose = sum(as.numeric(totalResult2) - 1) + (length(totalResult1) - sum(as.numeric(totalResult1) - 1))
      data_hist[(i-1)*30 + j, 3] = totalWin
      data_hist[(i-1)*30 + j, 4] = totalLose
      data_hist[(i-1)*30 + j, 5] = totalWin / totalGame
    }
  }
}

data_hist = data_hist[!is.na(data_hist$team1),]
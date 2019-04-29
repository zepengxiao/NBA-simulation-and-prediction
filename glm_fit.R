startMatchfit = min(which(data_boxscore$gmDate == "2017-12-01"))
endMatchfit = max(which(data_boxscore$gmDate == "2018-03-11"))

data_fit = as.data.frame(matrix(NA, endMatchfit - startMatchfit + 1, 9))
data_fit[,1] = data_boxscore$gmDate[startMatchfit:endMatchfit]
data_fit[,2] = data_boxscore$teamAbbr[startMatchfit:endMatchfit]
data_fit[,3] = data_boxscore$opptAbbr[startMatchfit:endMatchfit]
for (i in 1:nrow(data_fit)) {
  data_fit[i,4] = data_standings$homeWin[which(data_standings$stDate == data_fit[i,1] & data_standings$teamAbbr == data_fit[i, 2])] / 
    (data_standings$homeLoss[which(data_standings$stDate == data_fit[i,1] & data_standings$teamAbbr == data_fit[i, 2])] +
       data_standings$homeWin[which(data_standings$stDate == data_fit[i,1] & data_standings$teamAbbr == data_fit[i, 2])])
  data_fit[i,5] = data_standings$homeWin[which(data_standings$stDate == data_fit[i,1] & data_standings$teamAbbr == data_fit[i, 3])] / 
    (data_standings$homeLoss[which(data_standings$stDate == data_fit[i,1] & data_standings$teamAbbr == data_fit[i, 3])] +
       data_standings$homeWin[which(data_standings$stDate == data_fit[i,1] & data_standings$teamAbbr == data_fit[i, 3])])
  data_fit[i,6] = data_standings$lastTen[which(data_standings$stDate == data_fit[i,1] & data_standings$teamAbbr == data_fit[i, 2])] / 10
  data_fit[i,7] = data_standings$lastTen[which(data_standings$stDate == data_fit[i,1] & data_standings$teamAbbr == data_fit[i, 3])] / 10
  data_fit[i,8] = data_hist$hist_rate[which(data_hist$team1 == as.character(data_fit[i,2]) & data_hist$team2 == as.character(data_fit[i, 3]))]
}
colnames(data_fit) = c("gmDate", "homeTeam", "awayTeam", "homeTeamRate", "awayTeamRate", "homeTeamlast10", "awayTeamlast10", "histRate", "Result")
data_fit[,9] = data_boxscore$teamRslt[startMatchfit:endMatchfit]
#data_fit[,4] = as.numeric(data_fit[,4]) - 1
glm1 = glm(Result ~ homeTeamRate + awayTeamRate + homeTeamlast10 + awayTeamlast10 + histRate, family = binomial, data = data_fit)
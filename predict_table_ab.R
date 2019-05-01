library(dplyr)

data_A =  read.csv(file="/Users/shuogong/Desktop/STAT428/stat428_group6/table_a.csv")
data_B =  read.csv(file="/Users/shuogong/Desktop/STAT428/stat428_group6/table_B.csv")
data_A = data_A[,-1]
data_B = data_B[,-1]

totalNRows = nrow(data_B)
set.seed(665267179)

sim_record_East = as.data.frame(matrix(NA, 1000, 8))
colnames(sim_record_East) = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th")
sim_record_West = as.data.frame(matrix(NA, 1000, 8))
colnames(sim_record_West) = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th")
sim_record_number = as.data.frame(matrix(0, 30, 10))
colnames(sim_record_number) = c("teamname", "confname", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th")
sim_record_number$teamname = team_level
sim_record_number$confname = data_A$confname

# simulating
sim_time = 1000
for (i in 1:sim_time) {
  data_A_copy = data_A
  data_B_copy = data_B
  for (nrowdata_B_copy in 1:totalNRows) {
    #data_B_copy[nrowdata_B_copy, ]$`HomeTeam(A)`
    
    homeTeam = data_B_copy[nrowdata_B_copy, ]$`HomeTeam(A)`
    awayTeam = data_B_copy[nrowdata_B_copy, ]$`AwayTeam(B)`
    
    homeTeamRow = which(data_A_copy$teamname == homeTeam)
    awayTeamRow = which(data_A_copy$teamname == awayTeam)
    
    homeTeamdata_A_copy = data_A_copy[homeTeamRow, 2:26]
    awayTeamdata_A_copy = data_A_copy[awayTeamRow, 2:26]
    
    data_B_copy[nrowdata_B_copy, ][6:30] = data_A_copy[homeTeamRow, ][2:26]
    data_B_copy[nrowdata_B_copy, ][31:55] = data_A_copy[awayTeamRow, ][2:26]
    
    homeTeamRate = homeTeamdata_A_copy[2][[1]]
    awayTeamRate = awayTeamdata_A_copy[3][[1]]
    homeTeamlast10 = homeTeamdata_A_copy[4][[1]]
    awayTeamlast10 = awayTeamdata_A_copy[4][[1]]
    histRate = data_B_copy[nrowdata_B_copy, ][5][[1]]
    
    predictorDataFrame = data.frame(homeTeamRate = homeTeamRate, awayTeamRate = awayTeamRate, homeTeamlast10 = homeTeamlast10,
                                    awayTeamlast10 = awayTeamlast10, histRate = histRate)
    
    #predictStat = predict(glm1, predictorDataFrame)
    #ridge regression
    predictStat = predict.glmnet(rr, as.matrix(predictorDataFrame))[which(cv.out$lambda == cv.out$lambda.min)]
    
    threshold = exp(predictStat) / (1 + exp(predictStat))
    if (runif(1) < threshold) {
      result = 1
    } else {
      result = 0
    }
    
    data_B_copy[nrowdata_B_copy, ][56] = result
    
    homeRate = data_A_copy[homeTeamRow, ][3][[1]]
    homeMatch = data_A_copy[homeTeamRow, ][15][[1]]
    homeWinMatch = round(homeRate * homeMatch, digits=0)
    data_A_copy[homeTeamRow, ][15] = homeMatch + 1
    data_A_copy[homeTeamRow, ][3] = (homeWinMatch + result) / (homeMatch + 1)
    data_A_copy[homeTeamRow, ][18:26] = data_A_copy[homeTeamRow, ][17:25]
    data_A_copy[homeTeamRow, ][17] = data_B_copy[nrowdata_B_copy, ][56] + 1
    data_A_copy$last10[homeTeamRow] = sum(data_A_copy[homeTeamRow, 17:26] - 1) / 10
    
    
    awayRate = data_A_copy[awayTeamRow, ][4][[1]]
    awayMatch = data_A_copy[awayTeamRow, ][16][[1]]
    awayWinMatch = round(awayRate * awayMatch, digits=0)
    data_A_copy[awayTeamRow, ][16] = awayMatch + 1
    data_A_copy[awayTeamRow, ][4] = (awayWinMatch + (1 - result)) / (awayMatch + 1)
    data_A_copy[awayTeamRow, ][18:26] = data_A_copy[awayTeamRow, ][17:25]
    if (data_B_copy[nrowdata_B_copy, ][56] == 0) {
      data_A_copy[awayTeamRow, ][17] = 2
    } else {
      data_A_copy[awayTeamRow, ][17] = 1
    }
    data_A_copy$last10[awayTeamRow] = sum(data_A_copy[awayTeamRow, 17:26] - 1) / 10
    data_A_copy$totalmatch[homeTeamRow] = data_A_copy$totalmatch[homeTeamRow] + 1
    data_A_copy$totalmatch[awayTeamRow] = data_A_copy$totalmatch[awayTeamRow] + 1
  }
  
  # recording result
  data_A_copy_East = data_A_copy[data_A_copy$confname == "East",]
  data_A_copy_East = data_A_copy_East[order(-data_A_copy_East$homerate*data_A_copy_East$homematch-data_A_copy_East$awayrate*data_A_copy_East$awaymatch),]
  sim_record_East[i, ] = data_A_copy_East$teamname[1:8]
  
  data_A_copy_West = data_A_copy[data_A_copy$confname == "West",]
  data_A_copy_West = data_A_copy_West[order(-data_A_copy_West$homerate*data_A_copy_West$homematch-data_A_copy_West$awayrate*data_A_copy_West$awaymatch),] 
  sim_record_West[i, ] = data_A_copy_West$teamname[1:8]
  
  for (rank in 1:8) {
    sim_record_number[which(sim_record_number$teamname == data_A_copy_East$teamname[rank]), rank + 2] = 
      sim_record_number[which(sim_record_number$teamname == data_A_copy_East$teamname[rank]), rank + 2] + 1
    sim_record_number[which(sim_record_number$teamname == data_A_copy_West$teamname[rank]), rank + 2] = 
      sim_record_number[which(sim_record_number$teamname == data_A_copy_West$teamname[rank]), rank + 2] + 1
  }
}

sim_record_number$total = rowSums(sim_record_number[,3:10])
sim_record_number = sim_record_number[order(sim_record_number$confname, -sim_record_number$total, -sim_record_number$`1st`, 
                                            -sim_record_number$`2nd`, -sim_record_number$`3rd`, -sim_record_number$`4th`, 
                                            -sim_record_number$`5th`, -sim_record_number$`6th`, -sim_record_number$`7th`, 
                                            -sim_record_number$`8th`),]




nba = read.csv("NBA_train.csv")
nbaTest = read.csv("NBA_test.csv")
str(nba)

table(nba$W, nba$Playoffs) # Need 42 Wins to make to Playoffs

nba$PTSdiff = nba$PTS - nba$oppPTS
plot(nba$PTSdiff, nba$W)
WinsReg = lm(W ~ PTSdiff, data = nba)
summary(WinsReg) # W = 41 + 0.03259(PTSdiff); if W >= 42, then PTSdiff >= 30.67

PTSReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = nba)
summary(PTSReg)
PTSReg$residuals
AvgPTS = mean(nba$PTS)
SSE = sum(PTSReg$residuals^2)
RMSE = sqrt(SSE/nrow(nba))

PTSReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = nba) # Removed TOV
summary(PTSReg2)

PTSReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = nba) # Removed TOV and DRB
summary(PTSReg3)

PTSReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = nba) # Removed TOV, DRB, and BLK
summary(PTSReg4)
SSE4 = sum(PTSReg4$residuals^2)
RMSE4 = sqrt(SSE4/nrow(nba))


#Predictions
PTSPrediction = predict(PTSReg4, newdata = nbaTest)
SSEPredict = sum((PTSPrediction - nbaTest$PTS)^2)
SSTPredict = sum((mean(nba$PTS) - nbaTest$PTS)^2)
R2 = 1 - (SSEPredict/SSTPredict)
RMSEPredict = sqrt(SSEPredict/nrow(nbaTest))

nba = read.csv("NBA_train.csv")
str(nba)

# *A == something attempted
# x2 = 2 pointers, x3 = 3 pointers (R doesn't like variables starting with a number), ft = free throws
# orb = offensive rebound, tov = turn over

# wins vs making to playoffs
table(nba$W, nba$Playoffs)
# wins 35 games, occured 13 times, only 3 made it to playoffs
# about 42 games, high chance of making to playoffs

nba$ptsDiff = nba$PTS - nba$oppPTS
plot(nba$ptsDiff, nba$W)
    # looks like strong correlation
winsReg = lm(W ~ ptsDiff, data = nba)
summary(winsReg)
    # r-squared 0.9423 is very high
# w = 41 + 0.03259 * ptsDiff
# if want w >= 42 then 
# ptsDiff >= (42 - 41) / 0.03259 = 30.67

# want to build a model with other variables to drive up ptsDiff
pointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + TOV + STL, data=nba)
summary(pointsReg)
    # some significate, others less so
pointsReg$residual
SSE = sum(pointsReg$residuals^2)
    # ~ 28414623
rmse = sqrt(SSE / nrow(nba))
    # ~ 184.47
    
# lets remove some variables to make a better model
pointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + BLK + STL, data=nba)
summary(pointsReg2)
    # r-squared almost the same, but with less variables
pointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + BLK + STL, data=nba)
summary(pointsReg3)
pointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=nba)
summary(pointsReg4)
    # r-squared still the same, but with less variables
    
sse4 = sum(pointsReg4$residuals^2)
rmse4 = sqrt(sse4 / nrow(nba))
    # practically unchanged with first model
    
# lets make predictions
nbatest = read.csv("NBA_test.csv")
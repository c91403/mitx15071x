baseball = read.csv("baseball.csv")
str(baseball)

# want to replicate Moneyball, use subset of < 2002 data
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# want another var rd = run scored - runs against
moneyball$rd = moneyball$RS - moneyball$RA
str(moneyball)

# build model on win vs run difference
winning = lm(W ~ rd, data=moneyball)
summary(winning)
    # rd high coeff, r-squared = 0.88
    
# moneyball test
# w = 80.8814 + 0.1058 * rd
# want win >= 95
#  <==> 80.8814 + 0.1058 * rd >= 95
#  <==> rd >= (95 - 80.8814) / 0.1058 = 133.4
# very close to money ball claim of needing rd = 135

# quiz 2.3.2
# w = 80.8814 + 0.1058 * rd = 80.8814 + 0.1058 * (713 - 614)

str(moneyball);
# build model on runs scored regression
runsreg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(runsreg)
    # BA coeff is negative, doesn't make sense, must be a multi-collarity
    
runsreg = lm(RS ~ OBP + SLG, data=moneyball)
summary(runsreg)
    # about the same r-squared as before, but simplier with fewer variables
    # since OBP coeff is much higher than SLG coeff, and both on same scale
    # then OBP more important than SLG
    # OOBP = opponents on base percentage
    # OSLG = opponents slugging percentage
# using sample steps, can build a model on opponents runs allowed
    # runs allowed = -837.38 + 2913.60(OOBP) + 1514.29(OSLG)
    # r-quared of 0.91
    
# quiz 2.3.3
# obp = 0.311, slg = 0.405, using model, what is expected runs score
# runs = -804.63 + 2737.77 * obp + 1584.91 * slg
# -804.63 + 2737.77 * 0.311 + 1584.91 * 0.405

# if opponent oobp = 0.297, oslg = 0.370, what does runs allowed predict
# -837.38 + 2913.60 * 0.297 + 1514.29 * 0.37

# quiz 2.3.4
# runs = -804.63 + 2737.77 * obp + 1584.91 * slg
ec = c(0.338, 0.540, 1400000, 0, 0)
jg = c(0.391, 0.450, 1065000, 0, 0)
fm = c(0.369, 0.374, 295000,  0, 0)
gm = c(0.313, 0.447, 800000,  0, 0)
cp = c(0.361, 0.500, 300000,  0, 0)
data = cbind(ec, jg, fm, gm,cp)
for (i in 1:5) {
    mb = -804.63 + 2737.77 * data[1,i] + 1584.91 * data[2,i]
    data[4,i] = mb
    data[5,i] = data[3,i] / mb 
}
# want jg and cp since data[4,] highest if picking 2 with total budget of $1.5M

# quiz 2.3.5
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)
# Since one of the correlations is positive and the other is negative,
# this means that there does not seem to be a pattern between regular
# season wins and winning the playoffs.
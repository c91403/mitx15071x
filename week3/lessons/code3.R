polling = read.csv("PollingData.csv")
str(polling)

# only 45 stats in 2012
table(polling$Year)

# missing data
summary(polling)

# how to deal with missing data?
# 1. remove any row with missing data
#    no good since that's half the data
# 2. delete variables with missing data
#    no good want to retain the survay data
# 3. fill missing data with average values
#    may not work if average is close to zero
# 4. fill in missing based on non-missing values (multiple imputation)
#    if one syrvey is negative then use negative
#    runs will be be different if using a random seed, so fix it
#    used library('mice')

# for some reason Rcpp was a bitch to install, had to use the command $ R CMD INSTALL Rcpp
install.packages("Rcpp")
install.packages("mice")
library("mice")

# limit dataframe to just 4 important vars
simple = polling[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR")]
summary(simple)

set.seed(144)
# generate the the na
imputed = complete(mice(simple))

# replace the na
polling["Rasmussen"] = imputed["Rasmussen"]
polling["SurveyUSA"] = imputed["SurveyUSA"]
summary(polling)

# split the data
train = subset(polling, Year == 2004 | Year == 2008)
test = subset(polling, Year == 2012)
table(train$Republican)
#  0  1 
# 47 53
# to pick a baseline, pick the more frequent one: republican has 53%
# so assume republican is true

# or chose a smarter baseline: use the sign of the state
# use the sign() function of the rasmussen result
#  smartbaseline:{sign(ras)>0 -> repub, sign(ras)<0 -> democrat, sign(ras)==0 -> undecided}
table(sign(train$Rasmussen))

# compare smartbaseline with actual result
table(train$Republican, sign(train$Rasmussen))
 #     -1  0  1 
 # 0   42  1  4 
 # 1    0  1 52
    # 4 mistakes, 2 inconclusive
    # 42 where Ras smart baseline predicted Democrat would win and actually did win
    # 52 where Ras smart baseline predicted Rep would win and actually did win
    
# lets see if we can find any correlations (have to exlude state names)
cor(train[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR", "Republican")])
    # "Rasmussen", "SurveyUSA" have a high correlation, probably not useful to include both
    #  PropR is highliy correlated to Repub, so start with that
    
# build the model
mod1 = glm(Republican ~ PropR, data=train, family="binomial")
summary(mod1)

# type="response" means make probabilities
# create predictions using the training set for now,
# just exploring how our variables work with the training set so don't use newdata
pred1 = predict(mod1, type="response")
table(train$Republican, pred1 >= 0.5)
    # we made 4 mistakes, this is the same as baseline
    
# find another variable to include, pick one that has less correlation pairs
# {DiffCount, Ras} or {DiffCount, SurvUSA} have cor < 0.6
mod2 = glm(Republican ~ DiffCount + SurveyUSA, data=train, family="binomial")
pred2 = predict(mod1, type="response")
table(train$Republican, pred2 >= 0.5)
    # still got 2 mistakes
summary(mod2)
    # makes sense in that coeff are positive, AIC is small
    # problem is significance pretty low,
    
# how does our model apply on the test set?
table(test$Republican, sign(test$Rasmussen))
    # 4 mistakes, 2 inconclusive, this is what we'll compare our models against
    
testPred = predict(mod2, newdata=test, type="response")
table(test$Republican, testPred >= 0.5)
    # pretty good, just 1 error
    # using a higher t may not be worth it in this case
    
# where is out mistake
subset(test, testPred >= 0.5 & Republican == 0) 
    # in Florida, most polls picked repub, but Obama won
    # overall this is a good enough model
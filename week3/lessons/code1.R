quality = read.csv("quality.csv")
str(quality)

# our target variable
table(quality$PoorCare)

# baseline model: assume (PoorCare = 0), so accuracy = 98/131 = 0.7480916

# sample splitting tool for training and test sets
install.packages("caTools")
library("caTools")

# use a seed to match the lesson
set.seed(88)

# split so 75% of data in training set, 25% in testing, assign randomly
# split will keep 75% of PoorCare=true in training set and 75% PoorCare==true in test set
#  so that test set represents the training enough
split = sample.split(quality$PoorCare, SplitRatio=0.75);
split
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

# build model, family == binomial means logistic regression
qualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(qualityLog)
  # OfficeVisits, Narcotics have positive cooefficients, so they have an effect
  #   also both has at least 1 star
  # AIC is like adjusted R2, but can only compare with same dataset
  #   want minimal AIC

# type==response tells predict to return probabilities
predictTrain = predict(qualityLog, type="response")
summary(predictTrain)

# check the mean of the predicted vs good and poor care
tapply(predictTrain, qualityTrain$PoorCare, mean)
  #         0         1 
  # 0.1894512 0.4392246 
  # notice that P(predict$PoorCare == 1) > P(predict$PoorCare == 0)
  
# Quiz 4.1 (3.2.1)
# Create a logistic regression model to predict "PoorCare"
# using the independent variables "StartedOnCombination" and "ProviderCount".
# Use the training set we created in the previous video to build the model.
quiz41 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(quiz41)
# What is the coefficient for "StartedOnCombination"?
    # The coefficient value is positive, meaning that
    # positive values of the variable make the outcome of 1 more likely.
    # This corresponds to Poor Care. 
    
table(qualityTrain$PoorCare, predictTrain > 0.5)
table(qualityTrain$PoorCare, predictTrain > 0.7)
table(qualityTrain$PoorCare, predictTrain > 0.2)

# generate ROC curves
install.packages("ROCR")
library("ROCR")

# create roc line
rocrPred = prediction(predictTrain, qualityTrain$PoorCare)
rocrPerf = performance(rocrPred, "tpr", "fpr")
plot(rocrPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# quiz 7
predictTest = predict(qualityLog, type="response", newdata=qualityTest)
rocrPredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(rocrPredTest, "auc")@y.values)

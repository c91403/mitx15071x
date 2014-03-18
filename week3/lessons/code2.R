framingham = read.csv("framingham.csv")
str(framingham)

library("caTools")
set.seed(1000);

# create training and test sets
# typically want 50% to 80% of data in training set, we chose 65%
split = sample.split(framingham$TenYearCHD, SplitRatio=0.65)

train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

# create out model, "~ ." use all other variables as indep vars
# careful not to use "~ ." on sets with unique ids
framinghamLog = glm(TenYearCHD ~ ., data=train, family=binomial)
summary(framinghamLog)
# all the significant vars have positive coeffs

# create the prediction
predictTest = predict(framinghamLog, type="response", newdata=test)
table(test$TenYearCHD, predictTest > 0.5)

#     FALSE TRUE
#   0  1069    6
#   1   187   11

# rarely got val==TRUE (predictTest > 0.5) with our predictions
# predicted negative cases = TN + FP

# model accuracy of our model = (TN + FP) / N
(1069 + 6) / (1069+187+6+11)

# since (predictTest < 0.5) && 10yCHD==0 is the most frequent value in table
# use it as the baseline (true negative cases = TN + TP)

# baseline accuracy = (TN + TP) / N
(1069 + 11) / (1069+187+6+11)

# baseline and model accuracy too close, is the model useless? lets check

library("ROCR")
rocrPred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(rocrPred, "auc")@y.values)
    # auc = 0.74 on testing set
    
# results
# model strength: model rarely predicts 10yCHD risk above 50% (accuracy near baseline)
# model can differentiate low-risk from high-risk (auc=74%)
# important risk factors for CHD: smoking, cholesterol, systolic blood pressure, glucose
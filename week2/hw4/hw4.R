FluTrain = read.csv("FluTrain.csv")
str(FluTrain)

# which week corresponds to the highest percentage of ILI-related physician visits?
FluTrain[which.max(FluTrain$Queries),]

# Which week corresponds to the highest percentage of ILI-related query fraction?
FluTrain[which.max(FluTrain$ILI),]

# Plot the histogram of the dependent variable, ILI.
# What best describes the distribution of values of ILI?
hist(FluTrain$ILI, main="Percentage of ILI-related physician visits for the corresponding week")

# it is often useful to predict the logarithm of dependent variable instead of
# the dependent variable itself -- this prevents the small number of unusually
# large or small observations from having an undue influence on the sum of
# squared errors of predictive models.
# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?
plot(FluTrain$Queries, log(FluTrain$ILI), main="natural logarithm of ILI versus Queries")

# Based on our understanding of the data from the previous subproblem,
# which model best describes our estimation problem?

# call the regression model from the previous problem (Problem 2.1) FluTrend1 
# What is the training set R-squared value for FluTrend1 model?
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

# For a single variable linear regression model,
# there is a direct relationship between the R-squared and
# the correlation between the independent and the dependent variables.
# What is the relationship we infer from our problem?
Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation
Correlation^2
log(1/Correlation)
exp(-0.5*Correlation)
# It appears that Correlation^2 is equal to the R-squared value.
# It can be proved that this is always the case. 

FluTest = read.csv("FluTest.csv")

# dependent variable in our model is log(ILI),
# so PredTest1 would contain predictions of the log(ILI) value.
# We are instead interested in obtaining predictions of the ILI value.
# We can convert from predictions of log(ILI) to predictions of ILI
# via exponentiation, or the exp() function.
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

# What is our estimate for the percentage of ILI-related physician visits
# for the week of March 11, 2012?
FluTest
PredTest1[11]
  # which(FluTest$Week == "2012-03-11 - 2012-03-17")

# What is the relative error betweeen the estimate and the observed for that week?
# (Observed ILI - Estimated ILI)/Observed ILI
(FluTest[11, "ILI"] - PredTest1[11]) / FluTest[11, "ILI"]

# What is the Root Mean Square Error (RMSE) between our estimates and the
# actual observations for the percentage of ILI-related physician visits?
# rmse = sqrt(sse/nrow(FluTest))
# sse = sum((PredTest1 - FluTest$ILI)^2)
rmse = sqrt(sum((PredTest1 - FluTest$ILI)^2)/nrow(FluTest))
# same as sqrt(mean((PredTest1-FluTest$ILI)^2))

# We will build a variable called ILILag2 that contains
# the ILI value from 2 weeks before the current observation.
# use the "zoo" package, which provides a number of helpful methods for time series models.
install.packages("zoo")
library(zoo)

# create the ILILag2 variable in the training set:
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

# How many values are missing in the new ILILag2 variable?
summary(FluTrain)

# plot() function to plot the log of ILILag2 against the log of ILI.
plot(log(FluTrain$ILI), log(FluTrain$ILILag2), main="log of ILILag2 against the log of ILI")

# Train a linear regression model on the FluTrain dataset to predict the
# log of the ILI variable using the Queries variable as well as
# the log of the ILILag2 variable. Call this model FluTrend2.

# Which coefficients are significant at the p=0.05 level in this regression model?
# What is the R^2 value of the FluTrend2 model?
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

# To make predictions with our FluTrend2 model, we will also need to
# add ILILag2 to the FluTest data frame 
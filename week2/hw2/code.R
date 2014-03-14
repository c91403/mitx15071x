cc = read.csv("climate_change.csv")
str(cc)

# split the data into a training set, consisting of all the observations up to and including 2006
# and a testing set consisting of the remaining years
training = subset(cc, Year <= 2006)
testing = subset(cc, Year > 2006)

# build a linear regression model using all of the independent variables (except Year and Month)
# to predict the dependent variable Temp. Use the training set to build the model.
model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=training)
summary(model1)
  # R2 = 0.7509
  
# Which variables are significant in the model?
# We will consider a variable signficant only if the p-value is below 0.05.

# Compute the correlations between all the variables in the training set.
# Which of the following independent variables is N2O highly correlated
# with (absolute correlation greater than 0.7)?
abs(cor(training$N2O, training$MEI))
abs(cor(training$N2O, training$CO2))
abs(cor(training$N2O, training$CH4))
abs(cor(training$N2O, training$CFC.11))
abs(cor(training$N2O, training$CFC.12))
abs(cor(training$N2O, training$Aerosols))
abs(cor(training$N2O, training$TSI))

# Which of the following independent variables is CFC.11 highly correlated with?
abs(cor(training$CFC.11, training$MEI))
abs(cor(training$CFC.11, training$CO2))
abs(cor(training$CFC.11, training$CH4))
abs(cor(training$CFC.11, training$N2O))
abs(cor(training$CFC.11, training$CFC.12))
abs(cor(training$CFC.11, training$Aerosols))
abs(cor(training$CFC.11, training$TSI))
cor(training)

# build a model with only MEI, TSI, Aerosols and N2O.
model2 = lm(Temp ~ N2O + MEI + TSI + Aerosols, data=training)
summary(model2)
# coefficient of N2O in this reduced model
# Enter the model R2

# Use the step function in R to derive a new model, with the full model as the initial model
model3 = step(model1)
summary(model3)
# Enter the R2 value of the model produced by the step function
# Which of the following variable(s) were eliminated from the full model by the step function?

# Using the model produced from the step function,
# calculate temperature predictions for the testing data set, using the predict function.
pred = predict(model3, newdata=testing)
sse = sum((pred - testing$Temp)^2)
sst = sum((mean(training$Temp) - testing$Temp)^2)
r2 = 1 - sse/sst
r2
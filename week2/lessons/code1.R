wine = read.csv("wine.csv")

str(wine)

# 1 variable linear regression, price vs agst
model1 = lm(Price ~ AGST, data=wine)
summary(model1)
    # adjusted r-squared = adjust on independent variables, R-squared will decrease if you add a bad var

model1$residuals

# calculate the SSE
SSE = sum(model1$residuals ^ 2)
SSE
    # 5.73

# predict price using AGST and HarvestRain
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
    # cooefficent for HR os 0.00457
    # R-squared increased, thats a good variable to include in the model
SSE = sum(model2$residual^2)
SSE
    # 2.97, lower is better
    
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
    # R-squared better at 0.8294, this is better than before
SSE = sum(model3$residual^2)
SSE
    # 1.73, lower is better

# quiz 2.2.4
modelQuiz4 = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelQuiz4)
    # multiple r-squared = 0.3177
    # havest rain coeff = -4.971e-03
    # intercept coeff = 7.865e+00
    
# removed FrancePop, since it has no stars
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
    # Multiple R-squared:  0.8286 same
    # Adjusted R-squared increased, so could be better
    # Age now has 2 stars but didn't have any stars before --> correlation variable

# calculate the correlation between 2 variables
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)

# correlation between all variables in a table form
cor(wine)

model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
    # r-squared lowered, this is worse than model4
    # wrong high correlation variables can make coefficents to have the wrong sign
    
# quiz 2.2.5
cor(wine$HarvestRain, wine$WinterRain)
modelQuiz5 = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelQuiz5)

winetest = read.csv("wine_test.csv")
# create predictions with an existing model
predicttest = predict(model4, newdata = winetest)
predicttest
    # 6.768925 6.684910, our data points look pretty good since they match str(winetest) 
    
SSE = sum((winetest$Price - predicttest) ^2)
SST = sum((winetest$Price - mean(wine$Price))^2)
1 - SSE / SST
# pretty good out of sample r-squared, but our test set is pretty small, should use a bigger test set
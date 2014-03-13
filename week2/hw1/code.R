# load the data, 'state' is built into R
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)

# Plot all of the states' centers with latitude on the y axis (the "y" variable in our dataset)
# and longitude on the x axis (the "x" variable in our dataset).
# The shape of the plot should be the familiar outline of the United States! 
plot(statedata$x, statedata$y, type="p", main="xy coords", xlab="longitude", ylab="latitude")
    # cannot do doesn't want to plot
    
# Using the tapply command, determine which region of the
# US (West, North Central, South, or Northeast) has the highest average high
# school graduation rate of all the states in the region:
tapply(statedata$HS.Grad,statedata$state.region, mean)

# make a boxplot of the murder rate by region
# Which region has the highest median murder rate?
boxplot(statedata$Murder ~ statedata$state.region, main="murder rate")
tapply(statedata$Murder, statedata$state.region, median)

# there is an outlier in the Northeast region of the boxplot you just generated.
# Which state does this correspond to?
subset(statedata, state.name == "Delaware" | state.name == "Rhode Island" | state.name == "Maine" | state.name == "New York")
subset(statedata, state.region == "Northeast")

# build a model to predict life expectancy by state
# Build the model with all potential variables included
# (Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area).
model21 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(model21)

# Call the coefficient for income x (the answer to Problem 2.1).
# What is the interpretation of the coefficient x?
x = -2.180e-05

# plot a graph of life expectancy vs. income using the command:
plot(statedata$Income, statedata$Life.Exp)
    # Although the point in the lower right hand corner of the plot appears to be an outlier,
    # we observe a positive linear relationship in the plot. 
# This means that multicollinearity is probably the issue. 

# remove largest 'tvalue' one at a time up to 3 times to get new model
# remove Area
model31a = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(model31a)

# remove illiteracy
model31b = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(model31b)

# remove income
model31c = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(model31c)

# remove insignificant variables, the "Multiple R-squared" will always be worse,
# but only slightly worse.
# On the contrary, when we remove insignificant variables,
# the "Adjusted R-squred" will frequently be better.

# Take a look at the vector of predictions by using the predict function, (on't need to pass a "newdata")
predictions = predict(model31c)
# Which state do we predict to have the lowest life expectancy?
sort(predictions)
# Which state actually has the lowest life expectancy? 
statedata[which.min(statedata$Life.Exp),]

# Which state do we predict to have the highest life expectancy?
# Which state actually has the highest life expectancy?
sort(predictions)
statedata[which.max(statedata$Life.Exp),]

# look at the vector of residuals (the difference between the predicted and actual values).
sort(abs(model31c$residuals))
# For which state do we make the smallest absolute error?
# For which state do we make the largest absolute error?
# load the data, 'state' is built into R
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)

# Plot all of the states' centers with latitude on the y axis (the "y" variable in our dataset)
# and longitude on the x axis (the "x" variable in our dataset).
# The shape of the plot should be the familiar outline of the United States! 
plot(statedata$x, statedata$y)
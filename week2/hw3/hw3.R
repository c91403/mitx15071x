pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# How many students are there in the training set?
str(pisaTrain);

# Using tapply() on pisaTrain, what is the average reading test score of males?
# Of females?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Which variables are missing data in at least one observation in the training set?
summary(pisaTrain)

# Type the following commands into your R console to remove observations
# with any missing value from pisaTrain and pisaTest:
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# How many observations are now in the training set?
nrow(pisaTrain)
nrow(pisaTest)

# Factor variables are variables that take on a discrete set of values,
# like the "Region" variable in the state dataset from the first homework
# problem this week. This is an unordered factor because there isn't any
# natural ordering between the levels.

# Which of the following variables is an unordered factor with at least 3 levels?
# Which of the following variables is an ordered factor with at least 3 levels?
summary(pisaTrain$grade)
summary(pisaTrain$male)
summary(pisaTrain$raceeth)

# convert unordered factor -> ordered factor:
#  define one level as 'reference' level and add a binary variable for each of the remaining levels
# typically choose most frequent as reference level
# ie color:{red, green, blue}
#  -> set green as ref and add n-1=2 binaries {colorred=0/1, colorblue=0/1} (green is assumed if both 0)

# by default R selects the first level alphabetically ("American Indian/Alaska Native")
# as the reference level of our factor instead of the most common level ("White").
# Set the reference level of the factor by typing the following two lines in your R console:
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# build a linear regression model (call it lmScore) using the training set
# to predict readingScore using all the remaining variables
# R provides the shorthand notation "readingScore ~ ." to mean
# "predict readingScore using all the other variables in the data frame."
lmScore = lm(readingScore ~ ., data=pisaTrain)

# What is the Multiple R-squared value of lmScore on the training set?
summary(lmScore)

# What is the training-set root-mean squared error (RMSE) of lmScore?
# rmse = sqrt(sse/nrow(pisaTrain))
# sse = sum((points - pisaTrain$readingScore)^2)
# points = predict(lmScore, newdata=pisaTrain)
rmse = sqrt(sum((predict(lmScore, newdata=pisaTrain) - pisaTrain$readingScore)^2)/nrow(pisaTrain))
# same as 
#  rmse = sqrt(mean(lmScore$residuals^2))
# same as 
#  SSE = sum(lmScore$residuals^2)
#  RMSE = sqrt(SSE / nrow(pisaTrain))

# Consider two students A and B. They have all variable values the same,
# except that student A is in grade 11 and student B is in grade 9.
# What is the predicted reading score of student A minus the predicted
# reading score of student B?
# coeff_grade * (A - B)
29.542707 * (11 - 9)

# What is the meaning of the coefficient associated with variable raceethAsian?
#  The predicted survival for these two students will differ
#  by the coefficient on the variable raceethAsian.

# Using the "predict" function and supplying the "newdata" argument,
# use the lmScore model to predict the reading scores of students in pisaTest.
# Call this vector of predictions "predTest". 
predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)

# What is the range between the maximum and minimum predicted reading score on the test set?
637.7-353.2

# What is the sum of squared errors (SSE) of lmScore on the testing set?
# What is the root-mean squared error (RMSE) of lmScore on the testing set?
sse = sum((predTest - pisaTest$readingScore)^2)
sse
rmse = sqrt(sse / nrow(pisaTest))
rmse

# What is the predicted test score used in the baseline model?
# Remember to compute this value using the training set and not the test set.
baseline = mean(pisaTrain$readingScore)
baseline
# What is the sum of squared errors of the baseline model on the testing set? (SST)
sst = sum((baseline - pisaTest$readingScore)^2)
sst

# What is the test-set R-squared value of lmScore?
r2 = 1 - (sse / sst)
r2
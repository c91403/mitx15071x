parole = read.csv("parole.csv")

# How many parolees are contained in the dataset?
str(parole)

# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)

# Which variables in this dataset are unordered factors with at least three levels?
summary(parole)

# Using the as.factor() function, convert these variables to factors.
# Keep in mind that we are not changing the values, just the way R understands
# them (the values are still numbers).
# How does the output of summary() change for a factor variable
# as compared to a numerical variable?
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)

# split data
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
# Roughly what proportion of parolees have been allocated to the training and testing sets?

nrow(train)
nrow(test)

# Using glm (and remembering the parameter family="binomial"),
# train a logistic regression model on the training set.
# Your dependent variable is "violator",
# and you should use all of the other variables as independent variables.

# What variables are significant in this model?
# Significant variables should have a least one star,
# or should have a probability less than 0.05 (the column Pr(>|z|) in the summary output).
mod1 = glm(violator ~ ., data = train, family="binomial")
summary(mod1)

# Consider a parolee who is male,       male = 1
# of white race,                        race = 1
# aged 50 years at prison release,      age = 50
# from the state of Maryland,           state = 1
# served 3 months,                      time.served = 3
# had a maximum sentence of 12 months,  max.sentence = 12
# did not commit multiple offenses,     multiple.offenses = 0
# and committed a larceny.              crime = 2
q43a = names(parole)
q43b = c(1, 1, 50, 1, 3, 12, 0, 2, 0)
q43 = data.frame(q43a, q43b)
pred = predict(mod1, newdata=q43)
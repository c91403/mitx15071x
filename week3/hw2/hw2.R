baseball = read.csv("baseball.csv")
str(baseball)

# How many team/year pairs are there in the whole dataset?

# we removed several years with shorter-than-usual seasons.
# Using the table() function, identify the total number of
# years included in this dataset.
nrow(table(baseball$Year))

# only analyzing teams that made the playoffs,
# use the subset() function to replace baseball with
# a data frame limited to teams that made the playoffs 
# (so your subsetted data frame should still be called "baseball").
# How many team/year pairs are included in the new dataset?
baseball = subset(baseball, Playoffs == 1)
str(baseball)

# Which of the following has been the number of teams
# making the playoffs in some season?
table(baseball$Year)

# dd the predictor variable NumCompetitors to the baseball data frame
# contain the number of total teams making the playoffs in the year of a particular team/year pair
PlayoffTable = table(baseball$Year)
# use this stored table to look up the number of teams
# in the playoffs in the year of each team/year pair
# What best describes the output of names(PlayoffTable)?
PlayoffTable
names(PlayoffTable)

# Which function call returns the number of playoff teams in 1990 and 2001?
PlayoffTable(1990, 2001)
PlayoffTable(c(1990, 2001))
PlayoffTable("1990", "2001")
PlayoffTable(c("1990", "2001"))
PlayoffTable[1990, 2001]
PlayoffTable[c(1990, 2001)
PlayoffTable["1990", "2001"]
PlayoffTable[c("1990", "2001")]
# Because PlayoffTable is an object and not a function,
#  we look up elements in it with square brackets instead of parentheses.
# We build the vector of years to be passed with the c() function.
#  Because the names of PlayoffTable are strings and not numbers,
#  we need to pass "1990" and "2001". 

# which look up the number of teams in the playoffs for each team/year pair
# in the dataset, and store it as a new variable named
# NumCompetitors in the baseball data frame?
baseball$NumCompetitors = PlayoffTable(baseball$Year)
baseball$NumCompetitors = PlayoffTable[baseball$Year]
baseball$NumCompetitors = PlayoffTable(as.character(baseball$Year))
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
#  as.character() is needed to convert the Year variable in the dataset to a string

# Add the NumCompetitors variable to your baseball data frame.
# How many playoff team/year pairs are there in our dataset
# from years where 8 teams were invited to the playoffs?
table(baseball$NumCompetitors)

# Add a variable named WorldSeries, predict whether a team won the World Series; 
# in our dataset this is denoted with a RankPlayoffs value of 1. 
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
# 1 == won world series
# 0 == did not win
# How many observations do we have in our dataset where a team did NOT win the World Series?
table(baseball$WorldSeries)

# When we're not sure which of our variables are useful in predicting
# a particular outcome, it's often helpful to build bivariate models,
# which are models that predict the outcome using a single independent variable.

# Which of the following variables is a significant predictor of the
# WorldSeries variable in a bivariate logistic regression model?
# Note that you have to build 12 models to answer this question!
mod01 = glm(WorldSeries ~ Year, data = baseball, family = "binomial"); summary(mod01);
mod02 = glm(WorldSeries ~ RS, data = baseball, family = "binomial"); summary(mod02);
mod03 = glm(WorldSeries ~ RA, data = baseball, family = "binomial"); summary(mod03);
mod04 = glm(WorldSeries ~ W, data = baseball, family = "binomial"); summary(mod04);
mod05 = glm(WorldSeries ~ OBP, data = baseball, family = "binomial"); summary(mod05);
mod06 = glm(WorldSeries ~ SLG, data = baseball, family = "binomial"); summary(mod06);
mod07 = glm(WorldSeries ~ BA, data = baseball, family = "binomial"); summary(mod07);
mod08 = glm(WorldSeries ~ RankSeason, data = baseball, family = "binomial"); summary(mod08);
mod09 = glm(WorldSeries ~ OOBP, data = baseball, family = "binomial"); summary(mod09);
mod10 = glm(WorldSeries ~ OSLG, data = baseball, family = "binomial"); summary(mod10);
mod11 = glm(WorldSeries ~ NumCompetitors, data = baseball, family = "binomial"); summary(mod11);
mod12 = glm(WorldSeries ~ League, data = baseball, family = "binomial"); summary(mod12);

# Build a model using all of the variables that you
# found to be significant in the bivariate models.
# How many variables are significant in the combined model?
mod13 = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = "binomial");
summary(mod13);

# Often, variables that were significant in bivariate models are
# no longer significant in multivariate analysis due to correlation
# between the variables
# Which of the following variable pairs have a high degree of correlation (-0.8 < x > 0.8)
corlist = baseball[c("Year", "RA", "NumCompetitors", "RankSeason")]
cor(corlist)

# Build all six of the two variable models listed in the previous problem.
# Together with the four bivariate models, you should have
# 10 different logistic regression models.
mod14 = glm(WorldSeries ~ Year + RA, data = baseball, family = "binomial");             summary(mod14);
mod15 = glm(WorldSeries ~ Year + RankSeason, data = baseball, family = "binomial");     summary(mod15);
mod16 = glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = "binomial"); summary(mod16);
mod17 = glm(WorldSeries ~ RA + RankSeason, data = baseball, family = "binomial");       summary(mod17);
mod18 = glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = "binomial");   summary(mod18);
mod19 = glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = "binomial"); summary(mod19);

# Which model has the best AIC value (the minimum AIC value)?
moda = rep(NA,20)
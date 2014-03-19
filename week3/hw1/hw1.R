songs = read.csv("songs.csv")
str(songs)

# How many observations (songs) are from the year 2010?
nrow(subset(songs, year == 2010))
    # also table(songs$year)
    
# How many songs does the dataset include for which the artist name is "Michael Jackson"?
nrow(subset(songs, artistname == "Michael Jackson"))

# Which of these songs by Michael Jackson made it to the Top 10?
mj = subset(songs, artistname == "Michael Jackson")
mj[c("songtitle", "Top10")]

#  time signature (timesignature) is discrete
# What are the values of this variable that occur in our dataset?
# Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)

# Out of all of the songs in our dataset, the song with the highest
# tempo is one of the following songs. Which one is it?
songs[which.max(songs$tempo), "songtitle"]

# split the data into a training set "SongsTrain" consisting
# of all the observations up to and including 2009 song releases,
# and a testing set "SongsTest", consisting of the 2010 song releases.
songsTrain = subset(songs, year <= 2009)
songsTest = subset(songs, year == 2010)

# How many observations (songs) are in the training set?
str(songsTrain)

# outcome variable is "Top10"
# Since the outcome variable is binary, we will build a logistic regression model.
# We'll start by using all song attributes as our independent variables, which we'll call Model 1.
# we won't use the variables "year", "songtitle", "artistname", "songID" or "artistID".

# trick to exclude some vars
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[ , !(names(songsTrain) %in% nonvars) ]
songsTest = songsTest[ , !(names(songsTest) %in% nonvars) ]

mod1 = glm(Top10 ~ ., data=songsTrain, family = "binomial")

# Looking at the summary of your model, what is the value
# of the Akaike Information Criterion (AIC)?
summary(mod1)

# What is the correlation between the variables
# "loudness" and "energy" in the training set?
cor(songsTrain$loudness, songsTrain$energy)

# Model 2, in which we keep "energy" and omit "loudness",
# Model 3, in which we keep "loudness" and omit "energy".

# Create Model 2, which is Model 1 without the independent
# variable "loudness". This can be done with the following command:
mod2 = glm(Top10 ~ . - loudness, data=songsTrain, family=binomial)

# We just subtracted the variable loudness.
# We couldn't do this with the variables "songtitle" and "artistname",
# because they are not numeric variables,
# and we might get different values in the test set that the
# training set has never seen.
# But this approach will work when you want to remove variables
# that you could feasibly use in your model.

# Look at the summary of SongsLog2, and inspect the coefficient
# of the variable "energy". What do you observe?
summary(mod2)

# create Model 3, Look at the summary of Model 3 and inspect the
# coefficient of the variable "loudness".
# Remembering that higher loudness and energy both occur in songs
# with heavier instrumentation, do we make the same observation
# about the popularity of heavy instrumentation as we did with Model 2?
mod3 = glm(Top10 ~ . - energy, data=songsTrain, family=binomial)
summary(mod3)

# Make predictions on the test set using Model 3.
# What is the accuracy of Model 3 on the test set, using a threshold of 0.45?
predictTest = predict(mod3, type="response", newdata=songsTest)
table(songsTest$Top10, predictTest > 0.45)
  #    FALSE TRUE
  #  0   309    5
  #  1    40   19
# model accuracy of our model = (TN + TP) / N
(309 + 19) / nrow(songsTest)

# an easier model would be to pick the most frequent
# outcome (a song is not a Top 10 hit) for all songs.
# What would be the fraction of accuracy using this
# baseline model for the test set?
# baseline accuracy = (TN + FP) / N
(309 + 5) / nrow(sonsTest)

# How many songs does Model 3 correctly predict as Top 10 hits in 2010, using a threshold of 0.45?
# TP = 19
# How many non-hit songs does Model 3 predict will be Top 10 hits, using a threshold of 0.45?
# FP
5

# What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
# TP / (TP + FN) = 19 / (19 + 40) = 19/59
# What is the specificity of Model 3 on the test set, using a threshold of 0.45?
# TN / (TN + FP) = 309 / (309 + 5) = 309/314
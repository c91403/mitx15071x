cps = read.csv("CPSData.csv")

# How many interviewees are in the dataset?
str(cps)

# Among the interviewees with a value reported for the Industry variable,
# what is the most common industry of employment? Please enter the
# name exactly how you see it.
table(cps$Industry)

# For instance, sort(table(CPS$Region)) sorts the regions by the number of interviewees from that region.
# Which state has the fewest interviewees?
sort(table(cps$State))

# What proportion of interviewees are citizens of the United States?
table(cps$Citizenship)
(116639 + 7073) / (131302)

# number of interviewees are of Hispanic ethnicity, as captured by the Hispanic variable.
# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(cps$Race, cps$Hispanic)

# Which variables have at least one interviewee with a missing (NA) value?
summary(cps)

# The function is.na(CPS$Married) returns a vector of TRUE/FALSE values for
# whether the Married variable is missing. We can see the breakdown of whether
# Married is missing based on the reported value of the Region variable with
# the function table(CPS$Region, is.na(CPS$Married)).
# Which is the most accurate:
table(cps$Region, is.na(cps$Married))
table(cps$Sex, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))
table(cps$Citizenship, is.na(cps$Married))

# How many states had all interviewees living in a non-metropolitan area
# (aka they have a missing MetroAreaCode value)? For this question, treat the
# District of Columbia as a state (even though it is not technically a state).
table(cps$State, is.na(cps$MetroAreaCode))

# Which region of the United States has the largest proportion of
# interviewees living in a non-metropolitan area?
r = table(cps$Region, is.na(cps$MetroAreaCode))
p = r[,2] / (r[,1] + r[,2])

# Which state has a proportion of interviewees living in a
# non-metropolitan area closest to 30%?
sort(tapply(is.na(cps$MetroAreaCode), cps$State, mean))

metro = read.csv("MetroAreaCodes.csv")
country = read.csv("CountryCodes.csv")

# How many metropolitan areas are stored in MetroAreaMap?
str(metro)
# How many countries are stored in CountryMap?
str(country)

cps = merge(cps, metro, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
# What is the name of the variable that was added to the data
# frame by the merge() operation?
str(cps)

# How many interviewees have a missing value for the new metropolitan area variable?
# Note that all of these interviewees would have been removed from the
# merged data frame if we did not include the all.x=TRUE parameter.
table(is.na(cps$MetroArea))

# Which of the following metropolitan areas has the largest number of interviewees?
sort(table(cps$MetroArea))

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(cps$Hispanic,cps$MetroArea,  mean))

# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether
# an interviewee is Asian, determine the number of metropolitan areas in the
# United States from which at least 20% of interviewees are Asian.
sort(tapply(cps$Race == "Asian",cps$MetroArea,  mean))

# determine which metropolitan area has the smallest proportion of
# interviewees who have received no high school diploma.
sort(tapply(cps$Education == "No high school diploma", cps$MetroArea, mean, na.rm=TRUE), decreasing=TRUE)

# Merge in the country of birth information from the CountryMap
# data frame, replacing the CPS data frame with the result
cps = merge(cps, country, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

# What is the name of the variable added to the CPS data frame by this merge operation?
str(cps)
# How many interviewees have a missing value for the new country of birth variable?
table(is.na(cps$Country))

# Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(cps$Country))

# What proportion of the interviewees from the
# "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have
# a country of birth that is not the United States? For this computation,
# don't include people from this metropolitan area who have a missing country of birth.
table(cps$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", cps$Country == "United States")
1668 / (1668 + 3736)

# Which metropolitan area has the largest number (note -- not proportion) of
# interviewees with a country of birth in India?
# Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question.
sort(tapply(cps$Country == "India", cps$MetroArea, sum, na.rm=TRUE))
# In Brazil?
sort(tapply(cps$Country == "Brazil", cps$MetroArea, sum, na.rm=TRUE))
# Somalia
sort(tapply(cps$Country == "Somalia", cps$MetroArea, sum, na.rm=TRUE))
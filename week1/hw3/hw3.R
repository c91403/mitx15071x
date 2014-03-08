poll = read.csv("mitx15071x/week1/AnonymityPoll.csv")

# How many people participated in the poll?
str(poll)

# How many interviewees responded that they use a smartphone?
table(poll$Smartphone)
summary(poll$Smartphone)

# Which of the following are states in the Midwest census region?
sort(table(poll$State, poll$Region)[,1])

#Which was the state in the South census region with the largest number of interviewees?
sort(table(poll$State, poll$Region)[,3])

# How many interviewees reported neither Internet use nor smartphone use?
 gs = subset(poll, Smartphone==1 |Smartphone==0 | Info.On.Internet==1|Info.On.Internet==0);
 str(gs)
 #1002 - 965 = 37
poll$internet = as.numeric(poll$Info.On.Internet+poll$Worry.About.Info+poll$Privacy.Importance+poll$Anonymity.Possible+poll$Tried.Masking.Identity)
poll$internet2 = as.numeric(poll$internet>=0)
table(poll$internet2, poll$Smartphone, useNA="ifany")
table(poll$Internet.Use, poll$Smartphone, useNA="ifany")

# How many interviewees have a missing value for their Internet use?
table(poll$Internet.Use, useNA="ifany")
# How many interviewees have a missing value for their smartphone use?
table(poll$Smartphone, useNA="ifany")

# Use the subset function to obtain a data frame called "limited",
# which is limited to interviewees who reported Internet use or who reported smartphone use
limited = subset(poll, Internet.Use == 1 | Smartphone == 1)
str(limited)

# Which variables have missing values in the limited data frame?
for (i in 1:13) {
    summary(limited[,i], useNA="ifany")
}

# What is the average number of pieces of personal information on the Internet,
# according to the Info.On.Internet variable?
summary(limited$Info.On.Internet)

# How many interviewees reported a value of 0 for Info.On.Internet?
# How many interviewees reported the maximum value of 11 for Info.On.Internet?
table(limited$Info.On.Internet)

# What proportion of interviewees who answered the Worry.About.Info question
# worry about how much information is available about them on the Internet? 
table(limited$Worry.About.Info)
386/(386+404)

# What proportion of interviewees who answered the Anonymity.Possible
# question who think it is possible to be completely anonymous on the Internet?
table(limited$Anonymity.Possible)
278/(278+475)

# What proportion of interviewees who answered the Tried.Masking.Identity
# question have tried masking their identity on the Internet?
table(limited$Tried.Masking.Identity)
128/(128+656)

# What proportion of interviewees who answered the Privacy.Laws.Effective
# question find United States privacy laws effective?
table(limited$Privacy.Laws.Effective)
186/(186+541)

# What is the best represented age group in the population?
table(limited$Age)

# What is the largest number of interviewees that have exactly the same
# value in their Age variable AND the same value in their Info.On.Internet
# variable? In other words, what is the largest number of overlapping points
# in the plot plot(limited$Age, limited$Info.On.Internet)? 
which.max(table(limited$Age, limited$Info.On.Internet))
table(limited$Age, limited$Info.On.Internet)[36]

# what appears to be the functionality of the jitter command?
? jitter

# Use the tapply() function to obtain the summary of the Info.On.Internet
# value, broken down by whether an interviewee is a smartphone user.
tapply(limited$Info.On.Internet, limited$Smartphone, summary)

# What is the average Info.On.Internet value for smartphone users?

# Similarly use tapply to break down the Tried.Masking.Identity
# variable for smartphone and non-smartphone users.
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)

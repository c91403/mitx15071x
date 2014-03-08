# ./R-3.0.2/bin/R

sqrt2 = sqrt(2);
hourYears = 365 * 24;

# show memory variables
ls();

country = c("brazil", "china", "something", "bs", "last")
life = c(74,75,60,200, 50)
# To combine two vectors into a data frame, you should use data.frame.
data = data.frame(country, life)

population = c(199000, 1390000, 1240000, 7997, 318000)
# To add a variable to your data frame, you should use cbind.
data2 = cbind(data, population)

country = c("gr", "au")
life = c(200, 50)
population = c(7997, 318000)
data3 = data.frame(country,life,population)

# To combine two data frames with the same variable values, you should use rbind
newdata = rbind(data2, data3)

who = read.csv("mitx15071x/week1/WHO.csv");
str(who);
summary(who);

whoEurope = subset(who, region = "Europe");
# save to file
#write.csv(whoEurope, "mitx15071x/week1/whoEurope.csv");

ls();
# remove var
remove(whoEurope);
ls();

under15 = who$Under15;
mean(who$Under15);
sd(who$Under15);
summary(who$Under15);
which.min(who$Under15);
who$Country[86];
which.max(who$Under15);
who$Country[124];

plot(who$GNI, who$FertilityRate)
outliers = subset(who, GNI > 10,000 & FertilityRate > 2.5)
nrows(outliers)
outliers[c("Country","GNI","FertilityRate")]

table(who$Region);
# sort over60 by region, then apply mean on each region
tapply(who$Over60, who$Region, mean);
# remove missing values of na with na.rm=True
tapply(who$LiteracyRate, who$Region, min, na.rm=TRUE);

mean(who$Over60);
    # 11.16366
q2 = c("Japan", "United Arab Emirates", "Sierra Leone", "Cuba", "Luxembourg", "Mali")
q2sub = subset(who, Country = q2);
who$Country[which.min(who$Over60)];
    #  United Arab Emirates
who$Country[which.max(who$LiteracyRate)];
    # Cuba
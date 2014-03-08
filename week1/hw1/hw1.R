#  ./R-3.0.2/bin/R

# https://courses.edx.org/c4x/MITx/15.071x/asset/mvtWeek1.csv
#    ID: a unique identifier for each observation
#    Date: the date the crime occurred
#    LocationDescription: the location where the crime occurred
#    Arrest: whether or not an arrest was made for the crime (TRUE if an arrest was made, and FALSE if an arrest was not made)
#    Domestic: whether or not the crime was a domestic crime, meaning that it was committed against a family member (TRUE if it was domestic, and FALSE if it was not domestic)
#    Beat: the area, or "beat" in which the crime occurred. This is the smallest regional division defined by the Chicago police department.
#    District: the police district in which the crime occured. Each district is composed of many beats, and are defined by the Chicago Police Department.
#    CommunityArea: the community area in which the crime occurred. Since the 1920s, Chicago has been divided into what are called "community areas", of which there are now 77. The community areas were devised in an attempt to create socially homogeneous regions.
#    Year: the year in which the crime occurred.
#    Latitude: the latitude of the location at which the crime occurred.
#    Longitude: the longitude of the location at which the crime occurred.

mvt = read.csv("mvtWeek1.csv");

# How many rows of data (observations) are in this dataset?
summary(mvt);
nrow(mvt);
    # 191641
    
# How many variables are in this dataset?
str(mvt);
 #'data.frame':   191641 obs. of  11 variables:
 #$ ID                 : int  8951354 8951141 8952745 8952223 8951608 8950793 8950760 8951611 8951802 8950706 ...
 #$ Date               : Factor w/ 131680 levels "1/1/01 0:01",..: 42824 42823 42823 42823 42822 42821 42820 42819 42817 42816 ...
 #$ LocationDescription: Factor w/ 78 levels "ABANDONED BUILDING",..: 72 72 62 72 72 72 72 72 72 72 ...
 #$ Arrest             : logi  FALSE FALSE FALSE FALSE FALSE TRUE ...
 #$ Domestic           : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
 #$ Beat               : int  623 1213 1622 724 211 2521 423 231 1021 1215 ...
 #$ District           : int  6 12 16 7 2 25 4 2 10 12 ...
 #$ CommunityArea      : int  69 24 11 67 35 19 48 40 29 24 ...
 #$ Year               : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
 #$ Latitude           : num  41.8 41.9 42 41.8 41.8 ...
 #$ Longitude          : num  -87.6 -87.7 -87.8 -87.7 -87.6 ...
 
# What is the maximum value of the variable "ID"?
mvt$ID[which.max(mvt$ID)];
    # mvt[18134] = 9181151
# What is the minimum value of the variable "Beat"?
summary(mvt$Beat);
   #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    #111     722    1121    1259    1733    2535 
    
# How many observations have value TRUE in the Arrest variable (this is the number of arrests that were made)?
table(mvt$Arrest);
 #  FALSE   TRUE 
 # 176105  15536
 
 # How many observations have value "Alley" in the LocationDescription variable?
 pmatch("ALLEY", mvt$LocationDescription);
    # 46 (wrong)
length(grep("Alley", mvt$LocationDescription, ignore.case=TRUE))
    # 2311 (wrong)
mvt$isAlley = as.numeric(mvt$LocationDescription == "ALLEY")
table(mvt$isAlley)
    #      0      1 
    # 189333   2308 (correct)
    
# In what format are the entries in the variable Date?
head(mvt$Date)
[1] 12/31/12 23:15 12/31/12 22:00 12/31/12 22:00 12/31/12 22:00 12/31/12 21:30
[6] 12/31/12 20:30

# convert these characters into a Date object in R
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
# What is the month and year of the median date in our dataset?
summary(DateConvert)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2001-01-01" "2003-07-10" "2006-05-21" "2006-08-23" "2009-10-24" "2012-12-31" 

# extract the month and the day of the week, and add these variables to our data frame mvt
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
# replace the old Date variable with DateConvert by typing:
mvt$Date = DateConvert
# Using the table command, answer the following questions.
# In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)
#    April    August  December  February   January      July      June     March    May  November   October September 
#    15280     16572     16426     13511     16047     16801     16002     15758  16035     16063     17086     16060

# On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)
  # Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
  #  29284     27397     27118     26316     27319     26791     27416 
  
#In which month did the most crimes occur for which arrests were made?
table(mvt$Month, mvt$Arrest)
#            FALSE  TRUE
#  April     14028  1252
#  August    15243  1329
#  December  15029  1397
#  February  12273  1238
#  January   14612  1435 <--
#  July      15477  1324
#  June      14772  1230
#  March     14460  1298
#  May       14848  1187
#  November  14807  1256
#  October   15744  1342
#  September 14812  1248

# Problem 3.1
# First, let's make a histogram of the variable Date. We'll add an extra argument, to specify the number of bars we want in our histogram. In your R console, type
#png("mitx15071x/week1/p3.png")
#jpeg("mitx15071x/week1/p3.jpg")
hist(mvt$Date, breaks=100, main="3.1")
#dev.off()
# Looking at the histogram, answer the following questions.

boxplot(mvt$Date ~ mvt$Arrest, main="3.2")

# Problem 3.3
# For what fraction of motor vehicle thefts in 2001 was an arrest made? 
table(subset(mvt, Year=2001)$Arrest);
    #  FALSE   TRUE 
    # 176105  15536
100 * 15536 / (15536 + 176105)
    # 8.106825 (wrong)
table(mvt$Year, mvt$Arrest);
#        FALSE  TRUE
#  2001 18517  2152
#  2002 16638  2115
#  2003 14859  1798
100 * 2152 / (2152 + 18517)
    # 10.41173 (wrong)
table(mvt$Year, mvt$Arrest)["2001", 2] /  (table(mvt$Year, mvt$Arrest)["2001", 1] +  table(mvt$Year, mvt$Arrest)["2001", 2]);
# 0.1041173 (correct)

#For what fraction of motor vehicle thefts in 2007 was an arrest made? 
table(mvt$Year, mvt$Arrest)["2007", 2] /  (table(mvt$Year, mvt$Arrest)["2007", 1] +  table(mvt$Year, mvt$Arrest)["2007", 2]);
# 0.08487395

# For what fraction of motor vehicle thefts in 2012 was an arrest made? 
table(mvt$Year, mvt$Arrest)["2012", 2] /  (table(mvt$Year, mvt$Arrest)["2012", 1] +  table(mvt$Year, mvt$Arrest)["2012", 2]);
# 0.03902924

# Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?
sort(table(mvt$LocationDescription), decreasing=TRUE)[1:6]
    STREET PARKING LOT/GARAGE(NON.RESID.)   OTHER  ALLEY   GAS STATION DRIVEWAY - RESIDENTIAL
    156564                          14852    4573  2308          2111       1675

# Create a subset of your data, only taking observations for which the theft happened in one of these five locations, and call this new data set "Top5".
top5 = subset(mvt, LocationDescription == "STREET" |  LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription == "ALLEY" | LocationDescription == "GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL");
str(top5);
#'data.frame':   177510 obs. of  14 variables:

#  To make our tables a bit nicer to read, we can refresh this factor variable. 
top5$LocationDescription = factor(top5$LocationDescription);
str(top5);
# One of the locations has a much higher arrest rate than the other locations. Which is it?
r = table(top5$LocationDescription, top5$Arrest);
rate = r[,2] / (r[,1] + r[,2])
e = cbind(r, rate)
e
#                                FALSE  TRUE       rate
#ALLEY                            2059   249 0.10788562
#DRIVEWAY - RESIDENTIAL           1543   132 0.07880597
#GAS STATION                      1672   439 0.20795831
#PARKING LOT/GARAGE(NON.RESID.)  13249  1603 0.10793159
#STREET                         144969 11595 0.07405917

# On which day of the week do the most motor vehicle thefts at gas stations happen?
gs = subset(top5, LocationDescription=="GAS STATION");
table(gs$Weekday);
#   Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
#      332       280       338       336       282       270       273 

# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
table(top5$LocationDescription, top5$Weekday);
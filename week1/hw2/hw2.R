ibm = read.csv("mitx15071x/week1/IBMStock.csv")
ge = read.csv("mitx15071x/week1/GEStock.csv")
pg = read.csv("mitx15071x/week1/ProcterGambleStock.csv")
cc = read.csv("mitx15071x/week1/CocaColaStock.csv")
boe = read.csv("mitx15071x/week1/BoeingStock.csv")

ibm$Date = as.Date(ibm$Date, "%m/%d/%y")
ge$Date = as.Date(ge$Date, "%m/%d/%y")
cc$Date = as.Date(cc$Date, "%m/%d/%y")
pg$Date = as.Date(pg$Date, "%m/%d/%y")
boe$Date = as.Date(boe$Date, "%m/%d/%y")

# Our five datasets all have the same number of observations.
# How many observations are there in each data set? 
str(ibm)
# 480

# What is the earliest year in our datasets?
# 1970-01-01

# What is the latest year in our datasets?
tail(ibm)
#2009-12-01

# What is the mean stock price of IBM over this time period?
summary(ibm)
# Mean   :144.38

# What is the minimum stock price of General Electric (GE) over this time period?
summary(ge)
# Min.   :  9.294

# What is the maximum stock price of Coca-Cola over this time period?
summary(cc)
# Max.   :146.58

# What is the median stock price of Boeing over this time period?
summary(boe)
# Median : 44.88

# What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(pg$StockPrice)

# Around what year did Coca-Cola has its highest stock price in this time period?
cc[which.max(cc$StockPrice),]

# Around what year did Coca-Cola has its lowest stock price in this time period?
cc[which.min(cc$StockPrice),]

plot(cc$Date, cc$StockPrice, type='l'. col="red")
lines(pg$Date, pg$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

# In March of 2000, the technology bubble burst, and a stock market crash occurred.
# According to this plot, which company's stock dropped more?
# TODO: did not complete without graph


# 3.2 Which stock reaches the highest value in the time period 1995-2005?
ibm[432,]
ge[432,]
cc[432,]
pg[432,]
boe[432,]

# 4.1 Use the tapply command to calculate the mean stock price of IBM, sorted by months.
# To sort by months, use months(IBM$Date)
# as the second argument of the tapply function.
tapply(ibm$StockPrice, months(ibm$Date), mean)
mean(ibm$StockPrice)

# General Electric and Coca-Cola both have their highest
# average stock price in the same month. Which month is this?
g1 = tapply(ge$StockPrice, months(ge$Date), mean)
c1 = tapply(cc$StockPrice, months(cc$Date), mean)
gc = cbind(g1, c1)
gc
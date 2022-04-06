library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)


### Get the Symbol ###
getSymbols("AAPL", from = "2021-01-01", to ="2022-03-06")
na.omit(AAPL)

### Calculate Returns ###
CalculateReturns(AAPL)
return <- CalculateReturns(AAPL$AAPL.Close)
return <- return[-1]

### Moving Averages ###
sma <-SMA(Cl(AAPL),n=20)
ema <-EMA(Cl(AAPL),n=20)

### Bollinger Band ###
bb <-BBands(Cl(AAPL),s.d=2)
tail(bb, n=5)

### RSI ###
rsi = RSI(Cl(AAPL), n=14)
tail(rsi, n=5)

### Momentum and ROC ###
m <- momentum(Cl(AAPL), n=2)
roc <- ROC(Cl(AAPL),n=14)

## charting graph with indicators ###
chartSeries(AAPL,
            subset='2021-01::2022-01',
            theme=chartTheme('white'))
addROC(n=14, col = "red")
addRSI(n=14,maType="EMA")
addBBands(n=20,sd=2)
addSMA(n=20,on=1,col = "blue")
addSMA(n=200,on=1, col = "red")
addMomentum(n=1)

######### TRADING STRATEGIES #####
#1. Simple Filter Buy

getSymbols("AAPL")
price <- Cl(AAPL) # close price
r <- price/Lag(price) - 1 # % price change
delta <-0.005 #threshold
signal <-c(0) # first date has no signal

#Loop over all trading days (except the first)
for (i in 2: length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else
    signal[i]<- 0
}

# Assign time to action variable using reClass
signal<-reclass(signal,price)
tail(signal, n=3)

chartSeries(AAPL,
            subset="2021-08::2022-03-31",
            theme=chartTheme('white'))
addTA(signal,type='S',col='red')

trade <- Lag(signal,1) #trade based on yesterdays signal

ret<-dailyReturn(AAPL)*trade
names(ret)<-"filter"
charts.PerformanceSummary(ret, main="Naive Buy Rule")


#2. Simple Filter Buy and Sell
price <- Cl(MSFT)
r <- price/Lag(price) - 1
delta<-0.005
signal <-c(NA) # first signal is NA

for (i in 2: length(Cl(MSFT))){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(MSFT))

trade1 <- Lag(signal)
ret1<-dailyReturn(MSFT)*trade1
names(ret1) <- 'Naive'
charts.PerformanceSummary(ret1)

#3. RSI > 30
day <-14
price <- Cl(MSFT)
signal <- c()                    #initialize vector
rsi <- RSI(price, day)     #rsi is the lag of RSI
signal [1:day+1] <- 0            #0 because no signal until day+1

for (i in (day+1): length(price)){
  if (rsi[i] < 40){             #buy if rsi < 30
    signal[i] <- 1
  }else if (rsi[i]>65) {                       #no trade all if rsi > 30
    signal[i] <- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(MSFT))
trade2 <- Lag(signal)

#construct a new variable ret1
ret1 <- dailyReturn(MSFT)*trade1
names(ret1) <- 'Naive'
# construct a new variable ret2
ret2 <- dailyReturn(MSFT)*trade2
names(ret2) <- 'RSI'
retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall, 
                          main="Naive v.s. RSI")

chartSeries(AAPL,
            subset="2021-08::2022-04-01",
            theme=chartTheme('white'))
addTA(signal,type='S',col='red')

#4. Bollinger Band Trading Rule
#a. Simple 
day = 20
price <- Cl(MSFT)
signal <- c()
bb <- BBands(price,day,maType = "EMA", sd = 2)
signal [1:day+1] <- 0
r <- price/Lag(price) - 1 # % price change

for (i in (day+1): length(price)){
  if (price[i] >= bb$up[i]){
    signal[i]<- 1
  } else if (price[i] < bb$dn[i]){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(MSFT))
trade3 <- signal

ret3 <- dailyReturn(MSFT)*trade3
names(ret3) <- 'BB Simple'
retall <- cbind(ret1, ret2, ret3)
charts.PerformanceSummary(retall, 
                          main="Naive v.s. RSI v.s. BB Simple")
chartSeries(MSFT,
            subset="2021-08::2022-04-01",
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
addTA(signal,type='S',col='red')

#b. BB v.02 -> Buy when 2 consecutive 
day = 20
price = Cl(MSFT)
signal <- c()
bb <- BBands(price,day,maType = "EMA", sd = 2)
signal [1:day+1] <- 0
j <- i+1
k <- c()
k > j&i


######## Buy - Hold - Sell Rules ####### 
# Will be very good for the strategies I want to apply. 
# PART A
qty <-5
day <-14

signal <- c()    #trade signal with size
signal[1:(day+1)] <- 0 

price <- Cl(MSFT)

wealth <-c()
wealth[1:(day+1)] <- 100  

return<-c()                  
return[1:(day+1)] <- 0

profit <-c()
profit[1:(day+1)] <- 0

#PART B
rsi <- RSI(price, day)  #rsi is the lag of RSI
for (i in (day+1): length(price)){
  if (rsi[i] < 30){  #buy one more unit if rsi < 30
    signal[i] <- signal[i-1]+1
  } else if (rsi[i] < 50){  #no change if rsi < 50
    signal[i] <- signal[i-1] 
  } else {         #sell  if rsi > 50
    signal[i] <- 0
  }
}
signal<-reclass(signal,price)

# PART C
Close <- Cl(MSFT)
Open <- Op(MSFT)
trade <- Lag(signal)
for (i in (day+1):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret3<-reclass(return,price,profit,wealth)

charts.PerformanceSummary(ret3, main="Trade Size")


######### Non Day Trading ##############
# PART A
qty <-5
day <-14

signal <- c()   #trade signal
signal[1:(day+1)] <- 0 

price <- Cl(MSFT)

stock <- c()  #stock holding
stock[1:(day+1)] <-0

cash <-c()
cash[1:(day+1)] <- 100

# PART B
rsi <- RSI(price, day)  #rsi is the lag of RSI
for (i in (day+1): length(price)){
  if (rsi[i] < 30){  #buy one more unit if rsi < 30
    signal[i] <- 1
  } else if (rsi[i] < 50){ #no change if rsi < 50
    signal[i] <- 0
  } else {         #sell  if rsi > 50
    signal[i] <- -1
  }
}
signal<-reclass(signal,price)

#PART C
trade <- Lag(signal)    #rsi is the lag of RSI
for (i in (day+1): length(price)){
  if (trade[i]>=0){                                       
    stock[i] <- stock[i-1] + qty*trade[i]
    cash[i] <- cash[i-1] - 
      qty*trade[i]*price[i]
  } else{
    stock[i] <- 0
    cash[i] <- cash[i-1] + 
      stock[i-1]*price[i]
  }
}
stock<-reclass(stock,price)
cash<-reclass(cash,price)

# PART D
equity <-c()
equity[1:(day+1)] <- 100 

return<-c()                  
return[1:(day+1)] <- 0

for (i in (day+1): length(price)){
  equity[i] <- stock[i] * price[i] + cash[i]
  return[i] <- equity[i]/equity[i-1]-1
}
equity<-reclass(equity,price)
return<-reclass(return,price)

test <- cbind.data.frame(price,rsi,signal,trade,qty,stock,equity,return)
View(test)

charts.PerformanceSummary(return, 
                          main="Non-Day-Trading")


######## Establishing trading Strategies #########
#A. What will be the strategies?
  #a. 20/5 SMA & 20/5 EMA
  #b. 14/9/8 SMA & 14/9/8 EMA
  #c. RSI 30/70
  #d. RSI 45/55
  #e. Buy - Double upper bounds & Sell when CP crosses MA (BB)

#B. Calculate the basic quantities?

price <- Cl(GSPC)

sma20 <-SMA(Cl(GSPC),n=20)
sma14 <-SMA(Cl(GSPC),n=14)
sma9 <-SMA(Cl(GSPC),n=9)
sma8 <-SMA(Cl(GSPC),n=8)
sma5 <-SMA(Cl(GSPC),n=5)

ema20 <-EMA(Cl(GSPC),n=20)
ema14 <- EMA(Cl(GSPC),n=14)
ema9 <-EMA(Cl(GSPC),n=9)
ema8 <-EMA(Cl(GSPC),n=8)
ema5 <-EMA(Cl(GSPC),n=5)

rsi <- RSI(price, n=14)

bb1 <-BBands(price, n = 20, sd=2, maType = "EMA")
bb2 <-BBands(price, n = 20, sd=2, maType = "SMA")

#C. Calculate Strategies Independently 
#a. 20/5 SMA
qty <-10
day <-20

signal <- c()    #trade signal with size
signal[1:(day+1)] <- 0 

wealth <-c()
wealth[1:(day+1)] <- 100  

return<-c()                  
return[1:(day+1)] <- 0

profit <-c()
profit[1:(day+1)] <- 0

for (i in (day+1): length(price)){
  if (sma20[i] <= sma5[i]){  
    signal[i] <- 1
  } else if (sma20[i] > sma5[i]){  
    signal[i] <- -1
  } else {         
    signal[i] <- 0
  }
}
signal<-reclass(signal,price)

Close <- Cl(GSPC)
Open <- Op(GSPC)
trade <- Lag(signal)
for (i in (day+1):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret1<-reclass(return,price)

#b. EMA 20/5
qty <-10
day <-20

signal <- c()    #trade signal with size
signal[1:(day+1)] <- 0 

wealth <-c()
wealth[1:(day+1)] <- 100  

return<-c()                  
return[1:(day+1)] <- 0

profit <-c()
profit[1:(day+1)] <- 0

for (i in (day+1): length(price)){
  if (ema20[i] <= ema5[i]){  
    signal[i] <- 1
  } else if (ema20[i] > ema5[i]){  
    signal[i] <- -1
  } else {         
    signal[i] <- 0
  }
}
signal<-reclass(signal,price)

Close <- Cl(GSPC)
Open <- Op(GSPC)
trade <- Lag(signal)
for (i in (day+1):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret2<-reclass(return,price)

#c. SMA 14/9/8
qty <-10
day <-14

signal <- c()    #trade signal with size
signal[1:(day+1)] <- 0 

wealth <-c()
wealth[1:(day+1)] <- 100  

return<-c()                  
return[1:(day+1)] <- 0

profit <-c()
profit[1:(day+1)] <- 0

for (i in (day+1): length(price)){
  if ((sma14[i] <= sma9[i]) & (sma9[i] <= sma8[i])){  
    signal[i] <- 1
  } else if ((sma14[i] >= sma9[i]) & (sma9[i] >= sma8[i])){  
    signal[i] <- -1
  } else {         
    signal[i] <- 0
  }
}
signal<-reclass(signal,price)

Close <- Cl(GSPC)
Open <- Op(GSPC)
trade <- Lag(signal)
for (i in (day+1):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret3<-reclass(return,price)









retall <- cbind(ret1, ret2,ret3)
charts.PerformanceSummary(retall)
chartSeries(GSPC,
            subset="2021-01::2022-04-01",
            theme=chartTheme('white'))
addSMA(n=14,on=1,col = "blue")
addSMA(n=9,on=1, col = "pink")
addSMA(n=8,on=1, col = "purple")
addTA(signal,type='S',col='red')

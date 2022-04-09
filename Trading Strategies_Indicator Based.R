## Necessary Packages ##
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(plot.matrix)

### Section 1 - Indicator based Strategies ###

getSymbols("^GSPC")
price <- Cl(GSPC)              #Choosing SP500 

#Simple Moving Averages
sma20 <-SMA(Cl(GSPC),n=20)
sma14 <-SMA(Cl(GSPC),n=14)
sma9 <-SMA(Cl(GSPC),n=9)
sma8 <-SMA(Cl(GSPC),n=8)
sma5 <-SMA(Cl(GSPC),n=5)

#Exponential Moving Averages
ema20 <-EMA(Cl(GSPC),n=20)
ema14 <- EMA(Cl(GSPC),n=14)
ema9 <-EMA(Cl(GSPC),n=9)
ema8 <-EMA(Cl(GSPC),n=8)
ema5 <-EMA(Cl(GSPC),n=5)

#Relative Strength Index
rsi <- RSI(price, n=14)


##### TRADING STRATEGIES - ALL COMBINATIONS #####

# Desired Matrix will be of the following form
strat <- as.data.frame(matrix(1:36, nrow = 6, 
                   dimnames = list(c("20/5 SMA","20/5 EMA","14/9/8 SMA", "14/9/8 EMA","RSI 30", "RS1 45"), 
                                   c("20/5 SMA","20/5 EMA","14/9/8 SMA", "14/9/8 EMA","RSI 70", "RS1 55"))))
print(strat)


############################ 1. ##############################

qty <-1
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
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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

cum.ret1 <- Return.cumulative(ret1,geometric = TRUE)
ann.ret1 <- Return.annualized(ret1, geometric = TRUE)
semisd.ret1 <- SemiSD(ret1)

############################ 2. ##############################

qty <-1
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
  } else if (sma20[i] > sma5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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

cum.ret2 <- Return.cumulative(ret2,geometric = TRUE)
ann.ret2 <- Return.annualized(ret2, geometric = TRUE)
semisd.ret2 <- SemiSD(ret2)

############################ 3. ############################## 

qty <-1
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
  if ((sma14[i] <= sma9[i]) && (sma9[i] <= sma8[i])){  
    signal[i] <- 1
  } else if (sma20[i] > sma5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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

cum.ret3 <- Return.cumulative(ret3,geometric = TRUE)
ann.ret3 <- Return.annualized(ret3, geometric = TRUE)
semisd.ret3 <- SemiSD(ret3)

############################ 4. ##############################  

qty <-1
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
  if ((ema14[i] <= ema9[i]) && (ema9[i] <= ema8[i])){  
    signal[i] <- 1
  } else if (sma20[i] > sma5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret4<-reclass(return,price)

cum.ret4 <- Return.cumulative(ret4,geometric = TRUE)
ann.ret4 <- Return.annualized(ret4, geometric = TRUE)
semisd.ret4 <- SemiSD(ret4)

############################ 5. ##############################  

qty <-1
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
  if (rsi[i] <= 30){  
    signal[i] <- 1
  } else if (sma20[i] > sma5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret5<-reclass(return,price)

cum.ret5 <- Return.cumulative(ret5,geometric = TRUE)
ann.ret5 <- Return.annualized(ret5, geometric = TRUE)
semisd.ret5 <- SemiSD(ret5)

############################ 6. ##############################  

qty <-1
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
  if (rsi[i] <= 45){  
    signal[i] <- 1
  } else if (sma20[i] > sma5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret6<-reclass(return,price)

cum.ret6 <- Return.cumulative(ret6,geometric = TRUE)
ann.ret6 <- Return.annualized(ret6, geometric = TRUE)
semisd.ret6 <- SemiSD(ret6)

############################ 7. ##############################

qty <-1
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
  } else if (ema20[i] > ema5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret7<-reclass(return,price)

cum.ret7 <- Return.cumulative(ret7,geometric = TRUE)
ann.ret7 <- Return.annualized(ret7, geometric = TRUE)
semisd.ret7 <- SemiSD(ret7)

############################ 8. ##############################

qty <-1
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
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret8<-reclass(return,price)

cum.ret8 <- Return.cumulative(ret8,geometric = TRUE)
ann.ret8 <- Return.annualized(ret8, geometric = TRUE)
semisd.ret8 <- SemiSD(ret8)

############################ 9. ##############################

qty <-1
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
  if ((sma14[i] <= sma9[i]) && (sma9[i] <= sma8[i])){  
    signal[i] <- 1
  } else if (ema20[i] > ema5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret9<-reclass(return,price)

cum.ret9 <- Return.cumulative(ret9,geometric = TRUE)
ann.ret9 <- Return.annualized(ret9, geometric = TRUE)
semisd.ret9 <- SemiSD(ret9)

############################ 10. ##############################

qty <-1
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
  if ((ema14[i] <= ema9[i]) && (sma9[i] <= ema8[i])){  
    signal[i] <- 1
  } else if (ema20[i] > ema5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret10<-reclass(return,price)

cum.ret10 <- Return.cumulative(ret10,geometric = TRUE)
ann.ret10 <- Return.annualized(ret10, geometric = TRUE)
semisd.ret10 <- SemiSD(ret10)

############################ 11. ##############################

qty <-1
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
  if (rsi[i] < 30){  
    signal[i] <- 1
  } else if (ema20[i] > ema5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret11<-reclass(return,price)

cum.ret11 <- Return.cumulative(ret11,geometric = TRUE)
ann.ret11 <- Return.annualized(ret11, geometric = TRUE)
semisd.ret11 <- SemiSD(ret11)

############################ 12. ##############################

qty <-1
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
  if (rsi[i] < 45){  
    signal[i] <- 1
  } else if (ema20[i] > ema5[i]){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret12<-reclass(return,price)

cum.ret12 <- Return.cumulative(ret12,geometric = TRUE)
ann.ret12 <- Return.annualized(ret12, geometric = TRUE)
semisd.ret12 <- SemiSD(ret12)

############################ 13. ##############################

qty <-1
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
  } else if ((sma14[i] > sma9[i]) && (sma9[i] > sma8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret13<-reclass(return,price)

cum.ret13 <- Return.cumulative(ret13,geometric = TRUE)
ann.ret13 <- Return.annualized(ret13, geometric = TRUE)
semisd.ret13 <- SemiSD(ret13)


############################ 14. ##############################

qty <-1
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
  } else if ((sma14[i] > sma9[i]) && (sma9[i] > sma8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret14<-reclass(return,price)

cum.ret14 <- Return.cumulative(ret14,geometric = TRUE)
ann.ret14 <- Return.annualized(ret14, geometric = TRUE)
semisd.ret14 <- SemiSD(ret14)

############################ 15. ##############################

qty <-1
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
  if ((sma14[i] <= sma9[i]) && (sma9[i] <= sma8[i])){  
    signal[i] <- 1
  } else if ((sma14[i] > sma9[i]) && (sma9[i] > sma8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret15<-reclass(return,price)

cum.ret15 <- Return.cumulative(ret15,geometric = TRUE)
ann.ret15 <- Return.annualized(ret15, geometric = TRUE)
semisd.ret15 <- SemiSD(ret15)

############################ 16. ##############################

qty <-1
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
  if ((ema14[i] <= ema9[i]) && (ema9[i] <= ema8[i])){  
    signal[i] <- 1
  } else if ((sma14[i] > sma9[i]) && (sma9[i] > sma8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret16<-reclass(return,price)

cum.ret16 <- Return.cumulative(ret16,geometric = TRUE)
ann.ret16 <- Return.annualized(ret16, geometric = TRUE)
semisd.ret16 <- SemiSD(ret16)

############################ 17. ##############################

qty <-1
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
  if (rsi[i] < 30){  
    signal[i] <- 1
  } else if ((sma14[i] > sma9[i]) && (sma9[i] > sma8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret17<-reclass(return,price)

cum.ret17 <- Return.cumulative(ret17,geometric = TRUE)
ann.ret17 <- Return.annualized(ret17, geometric = TRUE)
semisd.ret17 <- SemiSD(ret17)

############################ 18. ##############################

qty <-1
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
  if (rsi[i] < 45){  
    signal[i] <- 1
  } else if ((sma14[i] > sma9[i]) && (sma9[i] > sma8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret18<-reclass(return,price)

cum.ret18 <- Return.cumulative(ret18,geometric = TRUE)
ann.ret18 <- Return.annualized(ret18, geometric = TRUE)
semisd.ret18 <- SemiSD(ret18)

############################ 19. ##############################

qty <-1
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
  } else if ((ema14[i] > ema9[i]) && (ema9[i] > ema8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret19<-reclass(return,price)

cum.ret19 <- Return.cumulative(ret19,geometric = TRUE)
ann.ret19 <- Return.annualized(ret19, geometric = TRUE)
semisd.ret19 <- SemiSD(ret19)

############################ 20. ##############################

qty <-1
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
  } else if ((ema14[i] > ema9[i]) && (ema9[i] > ema8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret20<-reclass(return,price)

cum.ret20 <- Return.cumulative(ret20,geometric = TRUE)
ann.ret20 <- Return.annualized(ret20, geometric = TRUE)
semisd.ret20 <- SemiSD(ret20)

############################ 21. ##############################

qty <-1
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
  if ((sma14[i] <= sma9[i]) && (sma9[i] <= sma8[i])){  
    signal[i] <- 1
  } else if ((ema14[i] > ema9[i]) && (ema9[i] > ema8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret21<-reclass(return,price)

cum.ret21 <- Return.cumulative(ret21,geometric = TRUE)
ann.ret21 <- Return.annualized(ret21, geometric = TRUE)
semisd.ret21 <- SemiSD(ret21)

############################ 22. ##############################

qty <-1
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
  if ((ema14[i] <= ema9[i]) && (ema9[i] <= ema8[i])){  
    signal[i] <- 1
  } else if ((ema14[i] > ema9[i]) && (ema9[i] > ema8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret22<-reclass(return,price)

cum.ret22 <- Return.cumulative(ret22,geometric = TRUE)
ann.ret22 <- Return.annualized(ret22, geometric = TRUE)
semisd.ret22 <- SemiSD(ret22)

############################ 23. ##############################

qty <-1
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
  if (rsi[i] < 30){  
    signal[i] <- 1
  } else if ((ema14[i] > ema9[i]) && (ema9[i] > ema8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret23<-reclass(return,price)

cum.ret23 <- Return.cumulative(ret23,geometric = TRUE)
ann.ret23 <- Return.annualized(ret23, geometric = TRUE)
semisd.ret23 <- SemiSD(ret23)

############################ 24. ##############################

qty <-1
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
  if (rsi[i] < 45){  
    signal[i] <- 1
  } else if ((ema14[i] > ema9[i]) && (ema9[i] > ema8[i])){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret24<-reclass(return,price)

cum.ret24 <- Return.cumulative(ret24,geometric = TRUE)
ann.ret24 <- Return.annualized(ret24, geometric = TRUE)
semisd.ret24 <- SemiSD(ret24)

############################ 25. ##############################

qty <-1
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
  } else if (rsi[i] > 70){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret25<-reclass(return,price)

cum.ret25 <- Return.cumulative(ret25,geometric = TRUE)
ann.ret25 <- Return.annualized(ret25, geometric = TRUE)
semisd.ret25 <- SemiSD(ret25)


############################ 26. ##############################

qty <-1
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
  } else if (rsi[i] > 70){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret26<-reclass(return,price)

cum.ret26 <- Return.cumulative(ret26,geometric = TRUE)
ann.ret26 <- Return.annualized(ret26, geometric = TRUE)
semisd.ret26 <- SemiSD(ret26)


############################ 27. ##############################

qty <-1
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
  if ((sma14[i] <= sma9[i]) && (sma9[i] <= sma8[i])){  
    signal[i] <- 1
  } else if (rsi[i] > 70){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret27<-reclass(return,price)

cum.ret27 <- Return.cumulative(ret27,geometric = TRUE)
ann.ret27 <- Return.annualized(ret27, geometric = TRUE)
semisd.ret27 <- SemiSD(ret27)

############################ 28. ##############################

qty <-1
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
  if ((ema14[i] <= ema9[i]) && (ema9[i] <= ema8[i])){  
    signal[i] <- 1
  } else if (rsi[i] > 70){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret28<-reclass(return,price)

cum.ret28 <- Return.cumulative(ret28,geometric = TRUE)
ann.ret28 <- Return.annualized(ret28, geometric = TRUE)
semisd.ret28 <- SemiSD(ret28)

############################ 29. ##############################

qty <-1
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
  if (rsi[i] < 30){  
    signal[i] <- 1
  } else if (rsi[i] > 70){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret29<-reclass(return,price)

cum.ret29 <- Return.cumulative(ret29,geometric = TRUE)
ann.ret29 <- Return.annualized(ret29, geometric = TRUE)
semisd.ret29 <- SemiSD(ret29)

############################ 30. ##############################

qty <-1
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
  if (rsi[i] < 45){  
    signal[i] <- 1
  } else if (rsi[i] > 70){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret30<-reclass(return,price)

cum.ret30 <- Return.cumulative(ret30,geometric = TRUE)
ann.ret30 <- Return.annualized(ret30, geometric = TRUE)
semisd.ret30 <- SemiSD(ret30)

############################ 31. ##############################

qty <-1
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
  } else if (rsi[i] > 55){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret31<-reclass(return,price)

cum.ret31 <- Return.cumulative(ret31,geometric = TRUE)
ann.ret31 <- Return.annualized(ret31, geometric = TRUE)
semisd.ret31 <- SemiSD(ret31)

############################ 32. ##############################

qty <-1
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
  } else if (rsi[i] > 55){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret32<-reclass(return,price)

cum.ret32 <- Return.cumulative(ret32,geometric = TRUE)
ann.ret32 <- Return.annualized(ret32, geometric = TRUE)
semisd.ret32 <- SemiSD(ret32)

############################ 33. ##############################

qty <-1
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
  if ((sma14[i] <= sma9[i]) && (sma9[i] <= sma8[i])){  
    signal[i] <- 1
  } else if (rsi[i] > 55){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret33<-reclass(return,price)

cum.ret33 <- Return.cumulative(ret33,geometric = TRUE)
ann.ret33 <- Return.annualized(ret33, geometric = TRUE)
semisd.ret33 <- SemiSD(ret33)

############################ 34. ##############################

qty <-1
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
  if ((ema14[i] <= ema9[i]) && (ema9[i] <= ema8[i])){  
    signal[i] <- 1
  } else if (rsi[i] > 55){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret34<-reclass(return,price)

cum.ret34 <- Return.cumulative(ret34,geometric = TRUE)
ann.ret34 <- Return.annualized(ret34, geometric = TRUE)
semisd.ret34 <- SemiSD(ret34)

############################ 35. ##############################

qty <-1
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
  if (rsi[i] < 30){  
    signal[i] <- 1
  } else if (rsi[i] > 55){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret35<-reclass(return,price)

cum.ret35 <- Return.cumulative(ret35,geometric = TRUE)
ann.ret35 <- Return.annualized(ret35, geometric = TRUE)
semisd.ret35 <- SemiSD(ret35)

############################ 36. ##############################

qty <-1
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
  if (rsi[i] < 45){  
    signal[i] <- 1
  } else if (rsi[i] > 55){  
    signal[i] <- 0
  } else {         
    signal[i] <- signal[i-1]
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
ret36<-reclass(return,price)

cum.ret36 <- Return.cumulative(ret36,geometric = TRUE)
ann.ret36 <- Return.annualized(ret36, geometric = TRUE)
semisd.ret36 <- SemiSD(ret36)


###### Collate Results - Buy/Sell Stratgy Matrices #####

# Creating Cumulative Return Matrix 

col1 <- rbind(cum.ret1,cum.ret2,cum.ret3,cum.ret4,cum.ret5,cum.ret6)
col2 <- rbind(cum.ret7,cum.ret8,cum.ret9,cum.ret10,cum.ret11,cum.ret12)
col3 <- rbind(cum.ret13,cum.ret14,cum.ret15,cum.ret16,cum.ret17,cum.ret18)
col4 <- rbind(cum.ret19,cum.ret20,cum.ret21,cum.ret22,cum.ret23,cum.ret24)
col5 <- rbind(cum.ret25,cum.ret26,cum.ret27,cum.ret28,cum.ret29,cum.ret30)
col6 <- rbind(cum.ret31,cum.ret32,cum.ret33,cum.ret34,cum.ret35,cum.ret36)

cum.ret.matrix <- cbind.data.frame(col1,col2,col3,col4,col5,col6)
colnames(cum.ret.matrix) <- c("20/5 SMA","20/5 EMA","14/9/8 SMA", "14/9/8 EMA","RSI 70", "RS1 55")
rownames(cum.ret.matrix) <- c("20/5 SMA","20/5 EMA","14/9/8 SMA", "14/9/8 EMA","RSI 30", "RS1 45")
View(cum.ret.matrix)

# Creating Annualised Return Matrix

col1 <- rbind(ann.ret1,ann.ret2,ann.ret3,ann.ret4,ann.ret5,ann.ret6)
col2 <- rbind(ann.ret7,ann.ret8,ann.ret9,ann.ret10,ann.ret11,ann.ret12)
col3 <- rbind(ann.ret13,ann.ret14,ann.ret15,ann.ret16,ann.ret17,ann.ret18)
col4 <- rbind(ann.ret19,ann.ret20,ann.ret21,ann.ret22,ann.ret23,ann.ret24)
col5 <- rbind(ann.ret25,ann.ret26,ann.ret27,ann.ret28,ann.ret29,ann.ret30)
col6 <- rbind(ann.ret31,ann.ret32,ann.ret33,ann.ret34,ann.ret35,ann.ret36)

ann.ret.matrix <- cbind.data.frame(col1,col2,col3,col4,col5,col6)
colnames(ann.ret.matrix) <- c("20/5 SMA","20/5 EMA","14/9/8 SMA", "14/9/8 EMA","RSI 70", "RS1 55")
rownames(ann.ret.matrix) <- c("20/5 SMA","20/5 EMA","14/9/8 SMA", "14/9/8 EMA","RSI 30", "RS1 45")
View(ann.ret.matrix)

# Creating SemiSD Matrix

col1 <- rbind(semisd.ret1,semisd.ret2,semisd.ret3,semisd.ret4,semisd.ret5,semisd.ret6)
col2 <- rbind(semisd.ret7,semisd.ret8,semisd.ret9,semisd.ret10,semisd.ret11,semisd.ret12)
col3 <- rbind(semisd.ret13,semisd.ret14,semisd.ret15,semisd.ret16,semisd.ret17,semisd.ret18)
col4 <- rbind(semisd.ret19,semisd.ret20,semisd.ret21,semisd.ret22,semisd.ret23,semisd.ret24)
col5 <- rbind(semisd.ret25,semisd.ret26,semisd.ret27,semisd.ret28,semisd.ret29,semisd.ret30)
col6 <- rbind(semisd.ret31,semisd.ret32,semisd.ret33,semisd.ret34,semisd.ret35,semisd.ret36)

semisd.ret.matrix <- cbind.data.frame(col1,col2,col3,col4,col5,col6)
colnames(semisd.ret.matrix) <- c("20/5 SMA","20/5 EMA","14/9/8 SMA", "14/9/8 EMA","RSI 70", "RS1 55")
rownames(semisd.ret.matrix) <- c("20/5 SMA","20/5 EMA","14/9/8 SMA", "14/9/8 EMA","RSI 30", "RS1 45")
View(semisd.ret.matrix)

#### Choose the best Trading Strategy and re-run the specific code to reset signal 

#### CHARTING ######
chartSeries(GSPC,
            subset="2021-01::2022-04-09",
            theme=chartTheme('white'))
addRSI(n=14)
addSMA(n=14,on=1,col = "blue")
addSMA(n=9,on=1, col = "purple")
addSMA(n=8,on=1,col = "pink")
addTA(signal,type='S',col='red')
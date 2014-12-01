library(xts)
library(fPortfolio)
library(timeSeries)
library(PerformanceAnalytics)
####
#Flexible Asset Allocation
####
location<-"home"
wd.home<-"~/Quant Trading/FlexibleAA/"
momdir.home<-"~/Quant Trading/Momentum/"
datadir.home<-"~/Quant Trading/datasets/"
quanttrading.home<-"~/Quant Trading/"
wd.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/FlexibleAA/"
momdir.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/FlexibleAA/"
datadir.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/datasets/"
taa.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/"
if (location=="home"){
    wd<-wd.home
    momdir<-momdir.home
    datadir<-datadir.home
    quanttradingdir<-quanttrading.home
} else {
    wd<-wd.work
    momdir<-momdir.work
    datadir<-datadir.work
    quanttradingdir<-taa.owrk
}
setwd(wd)
source(paste(wd,"faa.r",sep=""))
source(paste(momdir,"momentum.r",sep=""))
source(paste(quanttradingdir,"random_portfolio_returns_of_n_classes.r",sep=""))
load(paste(datadir,"bbreturnsdaily.rdata",sep=""))

sp500<-returns.daily[,"SP500"]/100
sp500<-sp500[complete.cases(sp500)]

sec.names<-c("S5FINL","S5HLTH","S5UTIL","S5COND","S5INFT","S5INDU","S5CONS","S5MATR","S5TELS","S5ENRS","STTSY")
data<-returns.daily[,sec.names]/100
data<-data[complete.cases(data),]
faa.results<-faa(data,lookback=c(10,4,4),wt=c(1,0,0),n.top=3,cash.col=11,data.type="returns")

faa.returns<-faa.results$returns
faa.returns<-faa.returns["/2014-10-31"]
sp500<-sp500[paste(index(faa.returns)[1],tail(index(faa.returns),1),"/")]
cat("faa","\n")
print(table.AnnualizedReturns(faa.returns))
print(maxDrawdown(faa.returns))
print(CalmarRatio(faa.returns))
cat("\n\n")
cat("Average Monthly Turnover","\n")
cat("faa:",faa.results$turnover.mean,"\n")
plot(faa.results$allocations[,1:10],main="Sector Allocations for FAA",ylim=c(0,1))
plot(faa.results$allocations[,11],main="STTSY Allocations for FAA",ylim=c(0,1))
plot(faa.results$turnover.monthly,ylim=c(0,1),main="Turnover for FAA",ylab="Percent")
abline(h=faa.results$turnover.mean)

print(table.AnnualizedReturns(sp500))
print(maxDrawdown(sp500))
print(CalmarRatio(sp500))

mcs_results<-mcs(faa.returns[,3:ncol(faa.returns)],1000,3)
plot(mcs_results[,"MaxDD"],mcs_results[,"Return"],col="gray",xlab="Max Drawdown",
     ylab="Annual Return",xlim=c(0,1),
     ylim=c(min(mcs_results[,"Return"],Return.annualized(faa.returns))-.01,max(mcs_results[,"Return"],Return.annualized(faa.returns))+.01),
     main="FAA v 10,000 Random Outcomes")
points(maxDrawdown(faa.returns)[1],Return.annualized(faa.returns)[1],col="blue",pch=2)
points(maxDrawdown(faa.returns)[2],Return.annualized(faa.returns)[2],col="black",pch=3)
points(maxDrawdown(faa.returns)[3:ncol(faa.returns)],Return.annualized(faa.returns)[3:ncol(faa.returns)],col="red",pch=4)
points(maxDrawdown(sp500),Return.annualized(sp500),col="green",pch=5)
legend("topright",inset=.05,legend=c("Random","FAA","EqWt","AC","SP500"),pch=c(1,2,3,4,5),
       col=c("gray","blue","black","red","green"))
text(maxDrawdown(faa.returns)[1]+.05,Return.annualized(faa.returns)[1],"FAA",col="blue")
text(maxDrawdown(faa.returns)[2]+.05,Return.annualized(faa.returns)[2],"EqWt",col="black")
text(maxDrawdown(sp500),Return.annualized(sp500),"SP500",col="green")
for (i in 3:ncol(faa.returns)){
    text(maxDrawdown(faa.returns)[i]+.05,Return.annualized(faa.returns)[i],sec.names[i-2],col="red")
}

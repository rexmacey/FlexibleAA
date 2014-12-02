#example of faa
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
wd.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/FlexibleAA/"
momdir.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/FlexibleAA/"
datadir.work<-"G:/Atlanta/All Departmental Data/Research Department/Asset Allocation/Tactical Asset Allocation Models/datasets/"

if (location=="home"){
    wd<-wd.home
    momdir<-momdir.home
    datadir<-datadir.home
} else {
    wd<-wd.work
    momdir<-momdir.work
    datadir<-datadir.work
}
setwd(wd)
source(paste(wd,"faa.r",sep=""))
source(paste(momdir,"momentum.r",sep=""))
load(paste(datadir,"bbreturnsdaily.rdata",sep=""))
sec.names<-c("SP500VAN","EAFE","EM","STTSY","TTLBND","COMMOD","REITVAN")
data<-returns.daily[,sec.names]/100
data<-data[complete.cases(data),]

ac.returns.monthly<-mom_ac_hist_ret(data,1,"returns")
ac.returns.monthly<-ac.returns.monthly["1997-10-31/2014-10-31"]

faa.results<-faa(data,lookback=c(4,4,4),wt=c(1,0,0),n.top=3,cash.col=4,data.type = "returns")
faa.returns<-faa.results$returns
Return.annualized(faa.returns)
maxDrawdown(faa.returns)

faa.results<-faa.tc(data,lookback=c(4,4,4),wt=c(1,0,0),n.top=3,cash.col=4,data.type = "returns",tc = 0.002)
faa.returns<-faa.results$returns
Return.annualized(faa.returns)
maxDrawdown(faa.returns)

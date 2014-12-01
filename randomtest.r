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
set.seed(101)
trial<-function(ac.returns.monthly,n.top){
    n.mos<-length(ac.returns.monthly[,1])
    n.ac<-ncol(ac.returns.monthly)
    #returns<-vector(mode="numeric",length=n.mos)
    returns<-ac.returns.monthly[,1] #just to create xts container with correct dates
    for (i in 1:n.mos){
        returns[i]<-mean(ac.returns.monthly[i,sample(seq(1,n.ac),n.top,replace=F)])
    }
    result<-list()
    result$ror<-Return.annualized(returns)
    result$sharpe<- result$ror/StdDev.annualized(returns)
    result$maxdd<-maxDrawdown(returns)
    result$calmar<-result$ror/result$maxdd
    return(result)
}

mcs<-function(ac.returns.monthly, n.trials,n.top){
    mc.results<-matrix(data=NA,nrow<-n.trials,ncol<-4,dimnames=list(seq(1,n.trials),c("Return","Sharpe","MaxDD","Calmar")))
    for (i in 1:n.trials){
        trial_result<-trial(ac.returns.monthly,n.top)
        mc.results[i,"Return"]<-trial_result$ror
        mc.results[i,"StdDev"]<-trial_result$sharpe
        mc.results[i,"MaxDD"]<-trial_result$maxdd
        mc.results[i,"Calmar"]<-trial_result$calmar
    }
    return(mc.results)
}

#mcs_results<-mcs(ac.returns.monthly,10000,3)
#save(mcs_results,file="mcs_results.rdata")
load("C:/Users/Rex/Documents/Quant Trading/FlexibleAA/mcs_results.rdata")
load("C:/Users/Rex/Documents/Quant Trading/AdaptiveAA/faavaaa.rdata")
faa.returns<-faa.results$returns
plot(mcs_results[,"MaxDD"],mcs_results[,"Return"],col="gray",xlab="Max Drawdown",
     ylab="Annual Return",xlim=c(0,1),ylim=c(-0.02,.14),main="FAA v 10,000 Random Outcomes")
points(maxDrawdown(faa.returns)[1],Return.annualized(faa.returns)[1],col="blue",pch=2)
points(maxDrawdown(faa.returns)[2],Return.annualized(faa.returns)[2],col="black",pch=3)
points(maxDrawdown(faa.returns)[3:9],Return.annualized(faa.returns)[3:9],col="red",pch=4)
legend("topright",inset=.05,legend=c("Random","FAA","EqWt","AC"),pch=c(1,2,3,4),
       col=c("gray","blue","black","red"))
text(maxDrawdown(faa.returns)[1]+.05,Return.annualized(faa.returns)[1],"FAA",col="blue")
text(maxDrawdown(faa.returns)[2]+.05,Return.annualized(faa.returns)[2],"EqWt",col="black")
for (i in 1:7){
    text(maxDrawdown(faa.returns)[i+2]+.05,Return.annualized(faa.returns)[i+2],sec.names[i],col="red")
}
Return.annualized(faa.returns)
maxDrawdown(faa.returns)
summary(mcs_results)

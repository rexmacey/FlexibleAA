#replicate Flexible Asset Allocation Paper by Keller and Putten
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
source(paste(wd,"FlexibleAA2.r",sep=""))
source(paste(momdir,"momentum.r",sep=""))

library(PerformanceAnalytics)
library(timeSeries)

benchreturns<-function(startdt,enddt,b.sec,b.wts){
  if (length(b.sec)==length(b.wts)){
    data<-returns.daily[,b.sec]/100
    data<-data[complete.cases(data),]
    b.wts.r<-matrix(rep(b.wts,nrow(data)),ncol=length(b.wts),nrow=nrow(data),byrow=T)
    b.return<-data*b.wts.r
    b.return<-apply(b.return,1,sum)
    b.return<-as.xts(b.return,match.to=data)
    b.return<-b.return[as.Date(index(b.return))>=startdt & as.Date(index(b.return))<=enddt]
    colnames(b.return)<-"Benchmark"
    return(b.return)
  } else {
    return("Error: Differing lengths of security list and weights")
  }
}


FlexRun<-function(sec.names,lookback,wt,n.top,cash.col,data.type="prices"){
  data<-returns.daily[,sec.names]/100
  #if (ncol(data)!=ncol(sec.names)){
  #  return("Error: Couldn't find all security names in data")
  #}
  data<-data[complete.cases(data),]
  Flex.results<-FlexAA(data,lookback,wt,n.top,cash.col,data.type)
  Flex<-Flex.results$returns[,1:2]
  ac.returns<-Flex.results$returns[,3:9]
  b1.sec<-c("SP500","TTLBND")
  b1.wts<-c(0.6,0.4)
  b2.sec<-c("SP500","EAFE","EM","TTLBND")
  b2.wts<-c(0.36,0.19,0.05,0.4)
  b1<-benchreturns(as.Date(min(index(Flex))),as.Date(max(index(Flex))),b1.sec,b1.wts)
  b2<-benchreturns(as.Date(min(index(Flex))),as.Date(max(index(Flex))),b2.sec,b2.wts)
  bench<-merge.xts(b1,b2)
  colnames(bench)<-c("60%SP500/40%Bnd","36%SP500/19%EAFE/5%EM/40%Bnd")
  Flex.results$bench<-bench
  cat("\n\n")
  cat(sec.names,"\n")
  cat("lookback",lookback)
  cat(" wt",wt)
  cat(" n.top",n.top,"\n")
  cat("From ",format(min(index(Flex)),"%m/%d/%Y")," thru ", format(max(index(Flex)),"%m/%d/%Y"),"\n")
  print(table.AnnualizedReturns(Flex))
  print(maxDrawdown(Flex))
  print(CalmarRatio(Flex))
  cat("Turnover ",round(Flex.results$turnover,3),"\n\n")
  cat("\nAsset class Performance\n")
  print(table.AnnualizedReturns(ac.returns))
  print(maxDrawdown(ac.returns))
  print(CalmarRatio(ac.returns))  
  cat("\nBenchmark Performance\n")
  print(table.AnnualizedReturns(bench))
  print(maxDrawdown(bench))
  print(CalmarRatio(bench))  
  cat("***************************","\n","\n")
  return(Flex.results)
}

Quickrun<-function(sec.names,lookback,wt,n.top,cash.col,data.type="prices"){
    data<-returns.daily[,sec.names]/100
    data<-data[complete.cases(data),]
    result<-list()
    FAA<-FlexAA(data,lookback,wt,n.top,cash.col,data.type)
    x<-FAA$returns[,1:2]
    result$sec.names<-sec.names
    result$lookback<-lookback
    result$wt<-wt
    result$n.top<-n.top
    result$cash.col<-cash.col
    result$start.date<-min(index(x))
    result$end.date<-max(index(x))
    result$annualized.return<-Return.annualized(x)
    result$max.drawdown<-maxDrawdown(x)
    return(result)
}

load(paste(datadir,"bbreturnsdaily.rdata",sep=""))

sec.names<-c("SP500VAN","EAFE","EM","STTSY","TTLBND","COMMOD","REITVAN")
Flex<-FlexRun(sec.names,lookback=c(4,4,4),wt=c(1,0,0),n.top=3,cash.col=4,data.type="returns")
Flex<-FlexRun(sec.names,lookback=c(4,4,4),wt=c(1,0,0),n.top=4,cash.col=4,data.type="returns")
#Flex<-FlexRun(sec.names,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=3,cash.col=4,data.type="returns")
test<-list()
i<-1
for (lb in c(3,4,5,6,9,10,12)){
    for (corwt in c(0,.25,.5,.75,1,1.25,1.5)){
        for (top in c(3,4,5,6)){
            test[[i]]<-Quickrun(sec.names,lookback=c(lb,lb,lb),wt=c(1,.5,corwt),n.top=top,cash.col=4,data.type="returns")
            i<- i+1
            cat(i,"\n")
        }
    }
}

summary<-matrix(NA,nrow=length(test),ncol=8)
colnames(summary)<-c("i","Ret","Bench","DD","Calmar","lb","corwt","top")
for (i in 1:length(test)){
    summary[i,1]<-i
    summary[i,"Ret"]<-test[[i]]$annualized.return[1]
    summary[i,"Bench"]<-test[[i]]$annualized.return[2]
    summary[i,"DD"]<-test[[i]]$max.drawdown[1]
    summary[i,"Calmar"]<-test[[i]]$annualized.return[1]/test[[i]]$max.drawdown[1]
    summary[i,"lb"]<-test[[i]]$lookback[1]
    summary[i,"corwt"]<-test[[i]]$wt[3]
    summary[i,"top"]<-test[[i]]$n.top
}
save(test,summary,file="testvariations.rdata")
write.csv(summary,file="summary.csv")
test[[which.max(summary[,"Calmar"])]]
test[[which.max(summary[,"Ret"])]]
test[[which.min(summary[,"DD"])]]
# sec.names<-c("TTLEQB","INTLEQ","EM","STTSY","TTLBND","COMMOD","REITVAN")
# Flex<-FlexRun(sec.names,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=3,cash.col=4,data.type="returns")
# 
# sec.names<-c("R3000","EUR","EM","STTSY","TTLBND","COMMOD","REITVAN","PAC")
# Flex<-FlexRun(sec.names,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=3,cash.col=4,data.type="returns")
# 
# sec.names<-c("SP500VAN","PAC","EM","STTSY","TTLBND","COMMOD","REITVAN","EUR")
# Flex<-FlexRun(sec.names,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=3,cash.col=4,data.type="returns")
# 
# sec.names<-c("R3000","EAFE","EM","STTSY","TTLBND","COMMOD","REITVAN")
# Flex<-FlexRun(sec.names,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=3,cash.col=4,data.type="returns")
# 
# sec.names<-c("R3000","EAFE","EM","STTSY","TTLBND","COMMOD","REITVAN","HYBND","LNGTSY")
# Flex<-FlexRun(sec.names,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=4,cash.col=4,data.type="returns")
# 
# sec.names<-c("R3000","EAFE","EM","STTSY","TTLBND","COMMOD","REITVAN")
# Flex<-FlexRun(sec.names,lookback=c(6,6,6),wt=c(1,.5,.5),n.top=3,cash.col=4,data.type="returns")
# 
# sec.names<-c("R3000","EAFE","EM","STTSY","TTLBND","COMMOD","REITVAN")
# Flex<-FlexRun(sec.names,lookback=c(12,12,12),wt=c(1,.5,.5),n.top=3,cash.col=4,data.type="returns")


security.master
# Examination of allocation
# allocations<-study.results$allocations
# plot(study.results$allocations,main="Allocations")
# bplt<-barplot(colMeans(allocations),main="Average Allocation",col="blue")
# text(x= seq(.75,by=1.2,length.out=7), y= colMeans(allocations)-.03, labels=as.character(round(colMeans(allocations),3)), xpd=TRUE,col="yellow")
# study.results$turnover
# ac.contribution<-ac.returns*allocations
# ac.contribution<-apply(1+ac.contribution,2,prod)^(12/nrow(ac.returns))-1
# bplt<-barplot(ac.contribution,main="Average Contribution to Return Per Year",col="blue")
# text(x= seq(.75,by=1.2,length.out=7), y= ac.contribution-.0025, labels=as.character(round(ac.contribution,3)), xpd=TRUE,col="yellow")

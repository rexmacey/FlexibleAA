#replicate Flexible Asset Allocation Paper by Keller and Putten
source("~/Quant Trading/FlexibleAA/FlexibleAA.r")
library(PerformanceAnalytics)
library(timeSeries)
wd.home<-"~/Quant Trading/FlexibleAA/"
setwd(wd.home)
load("FAA_Study.rdata")
rm(returns.daily)
prices.daily<-prices.daily[complete.cases(prices.daily),]
adjclosecol<-seq(6,ncol(prices.daily),6)
data<-prices.daily[,adjclosecol]
colnames(data)<-c("US","Intl","EM","ShtTsy","TtlBnd","Comm","REIT")
study.results<-FlexAA(data,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=3,cash.col=4)

#v. paper which is 1/3/2005-12/11/2012
#R=0.147m v=.092, Drawdown=-0.074 (p10)
ex11<-study.results$returns["2005-01-01/2012-12-31",1:2]
table.AnnualizedReturns(ex11)
table.Drawdowns(ex11)
chart.CumReturns(ex11)

#02Jan1998-03Jan2005  (p13)
#R=0.134,V=0.077,D=-0.059%
#Bench:R=8.3%,V=9.8%,D=-15.2%
exOOS<-study.results$returns["1998-01-01/2005-12-31",1:2]
table.AnnualizedReturns(exOOS)
table.Drawdowns(exOOS)

#chart on p 13
p13cht<-study.results$returns["1998-01-01/2012-12-31",1:2]
chart.CumReturns(p13cht)

#Entire period (not in paper)
FullSample<-study.results$returns[,1:2]
table.AnnualizedReturns(FullSample)
table.DownsideRisk(FullSample)
chart.CumReturns(FullSample)
charts.RollingPerformance(FullSample,width=36)


#comparisons to other benchmarks
b2<-study.results$returns[,"US"]*.6 + study.results$returns[,"TtlBnd"]*.4
b3<-study.results$returns[,"US"]*.36 + study.results$returns[,"TtlBnd"]*.4+
    study.results$returns[,"Intl"]*.19+study.results$returns[,"EM"]*.05
Full2<-merge.xts(study.results$returns[,1],b2)
Full3<-merge.xts(study.results$returns[,1],b3)
colnames(Full2)<-c("Strategy","60US/40Bnd")
colnames(Full3)<-c("Strategy","36US/40Bnd/19IN/5EM")
table.AnnualizedReturns(Full2)
table.DownsideRisk(Full2)
table.AnnualizedReturns(Full3)
table.DownsideRisk(Full3)

# data on underlying asset classes
ac.returns<-study.results$returns[,3:9]
table.AnnualizedReturns(ac.returns)
table.DownsideRisk(ac.returns)

# Examination of allocation
allocations<-study.results$allocations
plot(study.results$allocations,main="Allocations")
bplt<-barplot(colMeans(allocations),main="Average Allocation",col="blue")
text(x= seq(.75,by=1.2,length.out=7), y= colMeans(allocations)-.03, labels=as.character(round(colMeans(allocations),3)), xpd=TRUE,col="yellow")
study.results$turnover
ac.contribution<-ac.returns*allocations
ac.contribution<-apply(1+ac.contribution,2,prod)^(12/nrow(ac.returns))-1
bplt<-barplot(ac.contribution,main="Average Contribution to Return Per Year",col="blue")
text(x= seq(.75,by=1.2,length.out=7), y= ac.contribution-.0025, labels=as.character(round(ac.contribution,3)), xpd=TRUE,col="yellow")

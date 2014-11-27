#Flexible AA Study
FlexAA<-function(data,lookback=c(4,4,4),wt=c(1,.5,.5),n.top=.5,cash.col=1,data.type="prices"){
    # data xts of daily prices
    # cash.col columns which is the cash substitute when abs mom is negative
    lookback.mom<-lookback[1] ; lookback.vol<-lookback[2] ; lookback.cor<-lookback[3] # months to lookback to calc values
    wt.mom<-wt[1] ;    wt.vol<-wt[2] ;    wt.cor<-wt[3] #wts of ranks in scoreing
    if (n.top==.5) n.top<- trunc(ncol(data)/2) #if set to .5, use top half of asset classes
    
    source("~/Quant Trading/momentum.r")
    library(quantmod)
    library(xts)
    library(TTR)
    ac.ret<-mom_ac_hist_ret(data,lookback.mom,data.type)
    ac.ret.rank<-mom_ac_ret_rank(data,lookback.mom,data.type)
    ac.sd<-vol_ac_hist_sd(data,lookback.vol,data.type)
    ac.sd.rank<-vol_ac_sd_rank(data,lookback.vol,data.type) #ranks of each asset class
    ac.cor.mean<-cor_ac_hist_mean(data,lookback.cor,data.type)
    ac.cor.rank<-cor_ac_cor_rank(data,lookback.cor,data.type)
    #align them all if the lookbacks are different
    ac.ret<-ac.ret[(1+max(lookback)-lookback.mom):nrow(ac.ret),]
    ac.ret.rank<-ac.ret.rank[(1+max(lookback)-lookback.mom):nrow(ac.ret.rank),]
    ac.sd<-ac.sd[(1+max(lookback)-lookback.vol):nrow(ac.sd),]
    ac.sd.rank<-ac.sd.rank[(1+max(lookback)-lookback.vol):nrow(ac.sd.rank),]
    ac.cor.mean<-ac.cor.mean[(1+max(lookback)-lookback.cor):nrow(ac.cor.mean),]
    ac.cor.rank<-ac.cor.rank[(1+max(lookback)-lookback.cor):nrow(ac.cor.rank),]
    #score the ac for each month, lowest is best
    #ac.score<-ac.ret.rank*wt.mom+ac.sd.rank*wt.vol+ac.cor.rank*wt.cor
    ac.score<- -ac.ret/ac.sd*wt.mom + ac.cor.mean*wt.cor #negate to make lower better.
    ac.score.rank<-t(apply(ac.score,1,rank,ties.method="random"))
    ac.topn<-t(apply(ac.score.rank, 1, function(x) match(seq(1,n.top),x) ))
    colnames(ac.topn)<-paste("Rank" , 1:n.top, sep="")
    head(ac.topn)
    # implement abs ret. Repace those with negative momentum with cash 
    ac.topn.abs<-ac.topn
    ac.topn.ret<-t(sapply(seq(1,length(ac.topn[,1])),function(i) ac.ret[i,ac.topn[i,]])) # returns corresponding to top ranks
    neg.idx<-ac.topn.ret<0
    ac.topn.abs[neg.idx]<-cash.col  #invest in these asset classes for the subsequent month
    #calc returns for strategy
    ac.returns.monthly<-mom_ac_hist_ret(data,1,data.type)
    ac.returns.monthly<-ac.returns.monthly[row.names(ac.topn.abs)]
    # to deal with lag, remove first monthly return and last obs of asset class rank
    ac.topn.abs<-ac.topn.abs[-nrow(ac.topn.abs),] #remove last row. No data to calc the next month's return
    ac.returns.monthly<-ac.returns.monthly[-1,]
    benchmark.returns<-rowMeans(ac.returns.monthly) #equal weighted
    strategy.returns<-t(sapply(seq(1,length(ac.topn.abs[,1])),function(i) ac.returns.monthly[i,ac.topn.abs[i,]]))
    strategy.returns<-rowMeans(strategy.returns)
    study.returns<-merge.xts(strategy.returns,benchmark.returns,ac.returns.monthly)
    allocations<-ac.returns.monthly #container
    allocations[,]<-0 #set all values to zero
    for (i in 1:length(allocations[,1])){
        for (j in 1:n.top){
            allocations[i,ac.topn.abs[i,j]]<-allocations[i,ac.topn.abs[i,j]]+1/n.top
        }
    }
    transactions<-diff(as.matrix(allocations))
    turnover.monthly<-as.xts(rowSums(abs(transactions))/2)
    turnover.mean<-mean(turnover.monthly)
    result<-list()
    result$returns<-study.returns
    result$allocations<-as.timeSeries(allocations)
    result$topn<-ac.topn.abs
    result$turnover<-turnover.mean
    return(result)
}

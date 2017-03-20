# Jspinney
# Feb. 14, 2016
# file description: retrive MSCI index risk data
rm(list=ls())

library(Rbbg,verbose = FALSE)
library(xts,verbose = FALSE)
library(TTR,verbose = FALSE)
library(GARPFRM, verbose = FALSE)

con = blpConnect(verbose=FALSE)

###################
# developed markets
tickers = c("MXCA Index","MXUS Index","MXAT Index","MXAU Index","MXBE Index",
            "MXDK Index","MXFI Index","MXFR Index","MXHK Index","MXIE Index",
            "MXIL Index","MXIT Index","MXJP Index","MXNL Index","MXNZ Index",
            "MXNO Index","MXPT Index","MXSG Index","MXES Index","MXSE Index",
            "MXCH Index","MXGB Index")
tx = substring(tickers,3,4)
analysis.date = Sys.Date()
start.date = as.Date("1998-12-31")
hist = bdh(con,tickers,"px_last",start.date,analysis.date)
hist.df = unstack(hist,px_last~ticker)
hist.xts = xts(hist.df,order.by=as.Date(hist[(1:dim(hist.df)[1]),2]))
hist.xts = hist.xts[rowSums(is.na(hist.xts))!=dim(hist.df)[2],]
hist.xts = na.locf(hist.xts)
hist.xts = na.locf(hist.xts,fromLast = TRUE)
ret = na.omit(diff(log(hist.xts)))

#lt risk
est = array(NaN,dim=dim(ret))
for (i in 1:ncol(ret)) {
  ew =  EWMA(ret[,i],lambda=.999,initialWindow = 63)
  est[,i] = coredata(ew$estimate)
}
est = est*sqrt(252)
est.xts = xts(est,order.by=index(ret))

#DM lt risk mean
dm.lt.m <- mean(na.omit(est.xts))

msci.curr <- tail(est.xts,1)

png(filename="ltrisk.png",height=450,width=720,units="px",pointsize=12)
bp <- boxplot(est,ylab="Vol",
        las=2,col="darkslategray1",at=c(1:ncol(est))
        ,names=tx,main="Equity Volatility - Long Term",ylim=c(0,1))
xi <- 0.3 + seq(bp$n)

points(xi, msci.curr, col = "red", pch = 7)
legend( x="topright", 
        legend=c( "recent"),
        col=c("red"), lwd=1, lty=c(1,2), 
        pch=c(7) )
dev.off()

# mt risk
est = array(NaN,dim=dim(ret))
for (i in 1:ncol(ret)) {
  ew =  EWMA(ret[,i],lambda=.99,initialWindow = 63)
  est[,i] = coredata(ew$estimate)
}
est = est*sqrt(252)
est.xts = xts(est,order.by=index(ret))

#DM mt risk mean
dm.mt.m <- mean(na.omit(est.xts))

msci.curr <- tail(est.xts,1)

png(filename="mtrisk.png",height=450,width=720,units="px",pointsize=12)
bp <- boxplot(est,ylab="Vol",
              las=2,col="darkslategray1",at=c(1:ncol(est))
              ,names=tx,main="Equity Volatility - Medium Term",ylim=c(0,1))
xi <- 0.3 + seq(bp$n)

points(xi, msci.curr, col = "red", pch = 7)
legend( x="topright", 
        legend=c( "recent"),
        col=c("red"), lwd=1, lty=c(1,2), 
        pch=c(7) )
dev.off()

# st risk
est = array(NaN,dim=dim(ret))
for (i in 1:ncol(ret)) {
  ew =  EWMA(ret[,i],lambda=.94,initialWindow = 63)
  est[,i] = coredata(ew$estimate)
}
est = est*sqrt(252)
est.xts = xts(est,order.by=index(ret))

#DM st risk mean
dm.st.m <- mean(na.omit(est.xts))

msci.curr <- tail(est.xts,1)

png(filename="strisk.png",height=450,width=720,units="px",pointsize=12)
bp <- boxplot(est,ylab="Vol",
              las=2,col="darkslategray1",at=c(1:ncol(est))
              ,names=tx,main="Equity Volatility - Short Term",ylim=c(0,1))
xi <- 0.3 + seq(bp$n)

points(xi, msci.curr, col = "red", pch = 7)
legend( x="topright", 
        legend=c( "recent"),
        col=c("red"), lwd=1, lty=c(1,2), 
        pch=c(7) )
dev.off()

###################
# emerging markets
tickers = c("MXBR Index","MXCL Index","MXCN Index","MXCO Index","MXCZ Index",
            "MXEG Index","MXHU Index","MXIN Index","MXID Index","MXKR Index",
            "MXMY Index","MXMX Index","MXMA Index","MXGR Index","MXPE Index",
            "MXPH Index","MXPL index","MXRU Index","MXZA Index","TAMSCI Index",
            "MXTH Index","MXTR Index")
tx = substring(tickers,3,4)
tx[tx == "MS"] = "TA"
analysis.date = Sys.Date()
start.date = as.Date("1998-12-31")
hist = bdh(con,tickers,"px_last",start.date,analysis.date)
hist.df = unstack(hist,px_last~ticker)
hist.xts = xts(hist.df,order.by=as.Date(hist[(1:dim(hist.df)[1]),2]))
hist.xts = hist.xts[rowSums(is.na(hist.xts))!=dim(hist.df)[2],]
hist.xts = na.locf(hist.xts)
hist.xts = na.locf(hist.xts,fromLast = TRUE)
ret = na.omit(diff(log(hist.xts)))

#lt risk
est = array(NaN,dim=dim(ret))
for (i in 1:ncol(ret)) {
  ew =  EWMA(ret[,i],lambda=.999,initialWindow = 63)
  est[,i] = coredata(ew$estimate)
}
est = est*sqrt(252)
est.xts = xts(est,order.by=index(ret))

#EM lt risk mean
em.lt.m <- mean(na.omit(est.xts))

msci.curr <- tail(est.xts,1)

png(filename="ltrisk_em.png",height=450,width=720,units="px",pointsize=12)
bp <- boxplot(est,ylab="Vol",
              las=2,col="darkslategray1",at=c(1:ncol(est))
              ,names=tx,main="Equity Volatility - Long Term",ylim=c(0,1))
xi <- 0.3 + seq(bp$n)

points(xi, msci.curr, col = "red", pch = 7)
legend( x="topright", 
        legend=c( "recent"),
        col=c("red"), lwd=1, lty=c(1,2), 
        pch=c(7) )
dev.off()

# mt risk
est = array(NaN,dim=dim(ret))
for (i in 1:ncol(ret)) {
  ew =  EWMA(ret[,i],lambda=.99,initialWindow = 63)
  est[,i] = coredata(ew$estimate)
}
est = est*sqrt(252)
est.xts = xts(est,order.by=index(ret))

#EM mt risk mean
em.mt.m <- mean(na.omit(est.xts))

msci.curr <- tail(est.xts,1)

png(filename="mtrisk_em.png",height=450,width=720,units="px",pointsize=12)
bp <- boxplot(est,ylab="Vol",
              las=2,col="darkslategray1",at=c(1:ncol(est))
              ,names=tx,main="Equity Volatility - Medium Term",ylim=c(0,1))
xi <- 0.3 + seq(bp$n)

points(xi, msci.curr, col = "red", pch = 7)
legend( x="topright", 
        legend=c( "recent"),
        col=c("red"), lwd=1, lty=c(1,2), 
        pch=c(7) )
dev.off()

# st risk
est = array(NaN,dim=dim(ret))
for (i in 1:ncol(ret)) {
  ew =  EWMA(ret[,i],lambda=.94,initialWindow = 63)
  est[,i] = coredata(ew$estimate)
}
est = est*sqrt(252)
est.xts = xts(est,order.by=index(ret))

#EM st risk mean
em.st.m <- mean(na.omit(est.xts))

msci.curr <- tail(est.xts,1)

png(filename="strisk_em.png",height=450,width=720,units="px",pointsize=12)
bp <- boxplot(est,ylab="Vol",
              las=2,col="darkslategray1",at=c(1:ncol(est))
              ,names=tx,main="Equity Volatility - Short Term",ylim=c(0,1))
xi <- 0.3 + seq(bp$n)

points(xi, msci.curr, col = "red", pch = 7)
legend( x="topright", 
        legend=c( "recent"),
        col=c("red"), lwd=1, lty=c(1,2), 
        pch=c(7) )
dev.off()

# Diff b/w DM & EM risk mean
lt.d <- em.lt.m / dm.lt.m -1
mt.d <- em.mt.m / dm.mt.m -1
st.d <- em.st.m / dm.st.m -1
c(lt.d,mt.d,st.d)

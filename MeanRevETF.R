library(quantmod)
library(DSTrading)
library(tseries)
library(urca)
Sys.setenv(TZ = "EST5EDT")

#### reads/loads CSV into R ####
symblist <- t(read.csv("gicetf.csv", header = FALSE))

quotes <- new.env() # create new enviroment for data to be loaded to

sapply(symblist, function(x){
  try(
    suppressWarnings(getSymbols(
      x, src = 'yahoo',
      from = "2016-01-01",
      env=quotes)),
    silent=TRUE)
})

Close.data <- eapply(quotes, Ad)
Close.df <- data.frame(Close.data)
ETF.x <- as.xts(Close.df)
GICs <- as.xts(Close.df[,c(1,4,5,7:9,12:15)])
Weekend <- endpoints(ETF.x, on = "weeks", k =1)

jotest <- ca.jo(GICs, type = "trace", K = 2, ecdet = "none", spec = "longrun")

getSymbols("^GSPC", src = "yahoo", from = "2016-01-01")

#### 20-day trailing P-value ####
#PValue <- as.data.frame(rollapply(tail(as.ts(ETF.x),300), 20, function(u) adf.test(u,k=1)$p.value))
PValue <- as.data.frame(sapply(as.ts(ETF.x[Weekend]), function(u) adf.test(u,k=1)$p.value))#Weekly Px
fastK <- na.omit(do.call(merge, lapply(ETF.x, function(k) stoch(k, 14,3,3)$fastK)))
names(fastK) <- colnames(ETF.x)

### Determine 30-Day Average Price for Reversion Targets ###
ETFMean <- do.call(merge, lapply(ETF.x, function(x) na.omit(runMean(x,30))))
#ETFMean <- as.data.frame(lapply(ETF.x, function(x) na.omit(SMA(x,30)))) #Alternate Way

### Extract "trend" component from KEMD function ###
EMDtrend <-  do.call(merge, lapply(ETF.x, function(x) na.omit(KEMD(x,delta =.7,
                                                                    n = 20, bandFraction =.25,
                                                                    maType ="SMA")$trend)))

#EMDtrend <- as.data.frame(lapply(ETF.x, function(x) na.omit(KEMD(x,delta =.7,
#                                                                  n = 20, bandFraction =.25,
#                                                                  maType ="SMA")$trend))) #Alternate Way
names(EMDtrend) <- names(ETF.x)

EMD.mntm <- as.data.frame(lapply(ETF.x, function(x) na.omit(KEMD(x,delta =.7, n = 20,
                                                                  bandFraction =.25,
                                                                  maType ="SMA")$momentum)))
names(EMD.mntm) <- names(EMDtrend)
EMDMom <- as.xts(EMD.mntm)

SP5.EMD <- KEMD(GSPC[,6], delta = .7, n = 20, bandFraction =.25, maType ="SMA")

###300-day Moving Average of KEMD "trend" ###
EMD.SMA <- as.data.frame(lapply(EMDtrend, function(x) na.omit(SMA(x,300))))
names(EMD.SMA) <- names(EMDtrend)
EMDSMA <- as.xts(EMD.SMA)

SP5.SMA <- SMA(SP5.EMD$trend,300)

#### Calculate Probabilities of Move for Each ETF ####
#Determine price in Percentage Terms should Mean Reversion occur for use in probability#

Timeframe <- '2017:/'

pdf(file = "C:/Users/rodney/Dropbox/RCode/1. TC2K Format/MomPlus-TC2K/MeanReversionETF/EMD-Sectors.pdf", width = 12,
    height = 8.5, paper = 'USr')

layout(rbind(c(1,2), c(3,4), c(5,6)))
chart_Series(EMDtrend$XLB, name = "XLB Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$XLB.Adjusted, on = 1)",
                                                 "add_TA(EMDMom$XLB.Adjusted, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLE, name = "XLE Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$XLE, on = 1)",
                                                 "add_TA(EMDMom$XLE, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLF, name = "XLF Empricial Mode Decomposition", TA = c("add_TA(EMDSMA$XLF, on = 1)",
                                                 "add_TA(EMDMom$XLF, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLI, name = "XLI Empricial Mode Decomposition", TA = c("add_TA(EMDSMA$XLI, on = 1)",
                                                 "add_TA(EMDMom$XLI, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLK, name = "XLK Empricial Mode Decomposition", TA = c("add_TA(EMDSMA$XLK, on = 1)",
                                                  "add_TA(EMDMom$XLK, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLP, name = "XLP Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$XLP, on = 1)",
                                                 "add_TA(EMDMom$XLP, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLRE, name = "XLRE Empricial Mode Decomposition", TA = c("add_TA(EMDSMA$XLRE, on = 1)",
                                                   "add_TA(EMDMom$XLRE, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLU, name = "XLU Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$XLU, on = 1)",
                                                 "add_TA(EMDMom$XLU, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLV, name = "XLV Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$XLV, on = 1)",
                                                 "add_TA(EMDMom$XLV, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XLY, name = "XLY Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$XLY, on = 1)",
                                                 "add_TA(EMDMom$XLY, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$GLD, name = "GLD Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$GLD, on = 1)",
                                                  "add_TA(EMDMom$GLD, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$PPA, name = "PPA Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$PPA, on = 1)",
                                                 "add_TA(EMDMom$PPA, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$XME, name = "XME Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$XME, on = 1)",
                                                  "add_TA(EMDMom$XME, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$SPHB, name = "SPHB Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$SPHB, on = 1)",
                                                  "add_TA(EMDMom$SPHB, col = 4)"), subset = Timeframe)

chart_Series(EMDtrend$SPLV, name = "SPLV Empirical Mode Decomposition", TA = c("add_TA(EMDSMA$SPLV, on = 1)",
                                                   "add_TA(EMDMom$SPLV, col = 4)"), subset = Timeframe)

chart_Series(SP5.EMD$trend, name = "SP500 Empirical Mode Decomposition", TA = c("add_TA(SP5.SMA, on = 1)",
                                                    "add_TA(SP5.EMD$momentum, col = 4)"), subset = Timeframe)

dev.off()

####HALF LIFE OF MEAN REVERSION####
ETF.lag <- lag(ETF.x$XME, -1)
delta.ETF <- diff(ETF.x$XME)

df <- cbind(tail(ETF.x$XME,20),tail( ETF.lag,20),tail( delta.ETF,20))
df <- df[-1 ,] #remove first row with NAs

regress.results <- lm(tail(delta.ETF,20) ~ tail(ETF.lag,20), data = df)

lambda <- summary(regress.results)$coefficients[2]
half.life <- -log(2)/lambda
half.life

##Convert to xts to data frame format to combine##
fastK.df <- as.data.frame(last(fastK))
ETFMu.df <- as.data.frame(last(ETFMean))
ETFPx.df <- as.data.frame(last(ETF.x))


#SigReport <- as.data.frame(rbind(round(last(PValue),4), round(last(fastK)*100,3), round(last(ETFMean),2), round(last(ETF.x),2)))
SigReport <- as.data.frame(rbind(round(t(PValue),4), round(fastK.df,4)*100, round(ETFMu.df,4), round(ETFPx.df,4)))
row.names(SigReport) <- c("P-value", "fastK (OS<20,OB>80)", "Mean", "Last Px")
colnames(SigReport) <- sub(".Adjusted", "", colnames(SigReport))

View(SigReport)

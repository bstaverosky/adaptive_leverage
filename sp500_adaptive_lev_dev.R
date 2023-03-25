rm(list=ls())
# Install libraries if not installed
{
list.of.packages <- c("quantmod", 
                      "PerformanceAnalytics",
                      "xts",
                      "lubridate",
                      "knitr",
                      "kableExtra",
                      "ggplot2",
                      "ggthemes",
                      "xtable",
                      "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
}
# Load Libraries
{
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(xtable)
library(dplyr)
source("/home/bstaverosky/Documents/projects/adaptive_leverage/adhoc_functions.R")
}
### LOAD ASSET TO TRADE ###
asset <- "SPY"
asset <- getSymbols(asset, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
asset <- asset[,4]
names(asset) <- "Close"
asset$Close <- na.locf(asset$Close)

##### USER INPUTS #####
smathres <- 1
volthres <- 1
p2hthres <- 0.9
svoldays <- 65
lvoldays <- 252
ssmadays <- 21
lsmadays <- 200
entry    <- 252
exit     <- 252

##### Factor Computation #####
asset$stvol <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x<65){
    NA
  } else {
    sd(diff(log(tail(asset[1:x,"Close"],svoldays))), na.rm = TRUE)
  }
})
asset$ltvol <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x<500){
    NA
  } else {
    sd(diff(log(tail(asset[1:x,"Close"],lvoldays))), na.rm = TRUE)
  }
})   

asset$vol_rat <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x>=400){
    #sign(log(asset[x,"ltvol"]/asset[x,"stvol"]))
    asset[x,"stvol"]/asset[x,"ltvol"]
  } else {
    NA
  }
})


asset$sma_rat <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x>=200){
    #sign(log(mean(tail(asset[1:x,"Close"],21))/mean(tail(asset[1:x,"Close"],200))))
    mean(tail(asset[1:x,"Close"],ssmadays))/mean(tail(asset[1:x,"Close"],lsmadays))
  } else {
    NA
  }
})


#Price to all time high ratio
asset$p2h <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x>257){
    asset[x,"Close"]/max(asset[(x-252):(x-1),"Close"])
    #ifelse((asset[x,"Close"]/max(asset[(x-100):x,"Close"]))>.9,1,0)
    #ifelse((asset[x,"Close"]/max(asset[1:(x-1),"Close"]))>.9,1,0)
    
  } else {
    NA
  }
})

# N Day High Signal

asset$dh <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x<entry){
    NA
  } else {
    max(asset[(x-entry):x,"Close"])
  }
})

asset$dl <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x<entry){
    NA
  } else {
    min(asset[(x-exit):x,"Close"])
  }
})

#asset$dh <- lag(asset$dh,1)
#asset$dl <- lag(asset$dl,1)
asset$dhlsig <- NA

for(x in seq(nrow(asset))){
  #print(x)
  if(x<entry){
    asset[x,"dhlsig"] <- 0
  } else if(asset[x,"Close"][[1]]==asset[x,"dh"][[1]]|asset[(x-1),"dhlsig"][[1]]==1) {
    
    if(asset[x,"Close"][[1]]<asset[x,"dl"][[1]]){
      asset[x,"dhlsig"] <- 0
    } else {
      asset[x,"dhlsig"] <- 1
    }
  } else {
    asset[x,"dhlsig"] <- 0
  }
}
#asset$dhlsig <- 1
#asset$dhlsig <- lag(asset$dhlsig, 1)

asset$fwdret <- sapply(seq(nrow(asset)), FUN = function(x){
  if(x>=nrow(asset)-21){
    0
  } else {
    log(asset[x+22,"Close"][[1]]/asset[x+1,"Close"][[1]])
  }
})

asset$Return <- dailyReturn(asset$Close)

asset$smasig <- ifelse(asset$sma_rat>smathres,1,0)
asset$volsig <- ifelse(asset$vol_rat<volthres,1,0)
asset$p2hsig <- ifelse(asset$p2h>p2hthres,1,0)
asset$score <- rowSums(asset[,c("smasig","volsig","p2hsig")])

# asset$multiplier <- ifelse(asset$smasig == 1 & asset$volsig == 1 & asset$p2hsig == 1, 3,
#                     ifelse(asset$smasig == 1 & asset$volsig == 1 & asset$p2hsig == 0, 0.9,
#                     ifelse(asset$smasig == 0 & asset$volsig == 1 & asset$p2hsig == 1, 0.9,
#                     ifelse(asset$smasig == 1 & asset$volsig == 0 & asset$p2hsig == 1, 0.9,
#                     ifelse(asset$smasig == 1 & asset$volsig == 0 & asset$p2hsig == 0, 0.5,
#                     ifelse(asset$smasig == 0 & asset$volsig == 0 & asset$p2hsig == 1, 0.5,
#                     ifelse(asset$smasig == 0 & asset$volsig == 0 & asset$p2hsig == 0, 0,
#                     ifelse(asset$smasig == 0 & asset$volsig == 1 & asset$p2hsig == 0, 0,0))))))))
# 
# ##### LAGGED SIGNAL FOR ROBUSTNESS #####
# asset$multiplier <- lag(asset$multiplier, k=1)
# asset$strat <- asset$Return * asset$multiplier
          
##### LAGGED SIGNAL FOR ROBUSTNESS #####
asset$score <- stats::lag(asset$score, k=1)
asset$strat <- ifelse(asset$score==0,asset$Return*0.0,
               ifelse(asset$score==1,asset$Return*0.5,
               ifelse(asset$score==2,asset$Return*0.9,
               ifelse(asset$score==3,asset$Return*3,0))))

# Get Benchmark
bmk <- "SPY"
bmk <- getSymbols(bmk, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
bmk <- bmk[,4]
bmk$Benchmark_3X_Buy_and_Hold <- dailyReturn(bmk)*3
names(bmk) <- c("SPY.Close", "Benchmark_3X_Buy_and_Hold")

# Performance Figures

perfdf <- data.frame(
  a = InformationRatio(asset[,"strat"], bmk[,"Benchmark_3X_Buy_and_Hold"]),
  b = SharpeRatio.annualized(asset[,"strat"])[[1]],
  c = table.InformationRatio(R = asset[,"strat"], Rb = bmk[,"Benchmark_3X_Buy_and_Hold"], scale = 12)[[1]][[2]]*100,
  d = table.CaptureRatios(asset[,"strat"], bmk[,"Benchmark_3X_Buy_and_Hold"])[[1]],
  e = table.CaptureRatios(asset[,"strat"], bmk[,"Benchmark_3X_Buy_and_Hold"])[[2]],
  f = maxDrawdown(asset[,"strat"]),
  g = max(table.Drawdowns(asset[,"strat"])$Length),
  h = AverageDrawdown(asset[,"strat"]),
  i = AverageLength(asset[,"strat"]),
  j = cor(asset$score, asset$fwdret, use = "complete.obs"),
  k = get_hitrate(asset$Return, asset$score),
  l = get_hitrate(asset$fwdret, asset$score),
  m = mean(asset$strat),
  n = NA,
  o = NA,
  p = NA,
  q = NA,
  r = NA,
  s = NA,
  t = NA
)
names(perfdf) <- c(
  "Information Ratio",
  "Sharpe Ratio",
  "Tracking Error",
  "Up Capture Ratio",
  "Down Capture Ratio",
  "Max Drawdown",
  "Max Drawdown Length",
  "Average Drawdown",
  "Average Drawdown Length",
  "Signal 1 Month Forward Return Correlation",
  "Daily Return Hitrate",
  "Monthly Return Hitrate",
  "Average Daily Return",
  "Win Rate",
  "Lose Rate",
  "Average Win Trade Return",
  "Average Loss Trade Return",
  "Win Loss Ratio",
  "Average Win Loss Ratio",
  "Win Trade to Loss Trade Spread"
)
perfdf <- t(perfdf)
names(perfdf) <- "Performance Statistics"

strat <- merge(asset[,c("strat", "Return")],bmk[,"Benchmark_3X_Buy_and_Hold"])

### CONVERT TO MONTHLY DATA ###

mstrat <- apply.monthly(strat, Return.cumulative)

if(tail(asset$score,1)==3){
  print("Strategy is currently 3X levered")
} else if(tail(asset$score,1)==2){
  print("Strategy is currently 0.9 levered")
} else if(tail(asset$score,1)==1){
  print("Strategy is currently 0.5 levered")
} else if(tail(asset$score,1)==0){
  print("Strategy is currently 100% cash")
} 

## Total Backtest Performance
output <- merge(asset[,c("strat", "Return")],bmk[,"Benchmark_3X_Buy_and_Hold"])
names(output) <- c("AdaptiveLeverage", "S&P500", "3X S&P500 Buy and Hold")
charts.PerformanceSummary(output, main = "Adaptive Leverage Strategy Performance")
print(Sys.time())



## Trailing 1-Year Performance

output <- tail(merge(asset[,c("strat", "Return")],bmk[,"Benchmark_3X_Buy_and_Hold"]), 256)
names(output) <- c("AdaptiveLeverage", "S&P500", "3X S&P500 Buy and Hold")
charts.PerformanceSummary(output, main = "Adaptive Leverage Strategy Performance")

## Signal Metrics
output <- as.data.frame(asset)
output$Date <- row.names(output)
output <- output[,c("vol_rat", "sma_rat", "p2h", "score")]
names(output) <- c("Volatility Ratio", "SMA Ratio", "Price-to-52 Week High", "Score")
output <- tail(output, 10)

kable(output,
      digits = 4,
      "html",
      booktabs = T) %>%
  kable_styling(latex_options = c("striped"), full_width = FALSE) %>%
  column_spec(1, width = "8cm")

## Performance Metrics

kable(perfdf,
      digits = 4,
      "html",
      booktabs = T) %>%
  kable_styling(latex_options = c("striped"), full_width = FALSE) %>%
  column_spec(1, width = "8cm")
#xtable(round(perfdf,4))



## Annualized Return

retdf <- lapply(c(252,756,1260,1764,2520,3780,5040), FUN = function(x){
  stratret <- Return.annualized(tail(strat[,"strat"],x), scale = 252)
  bmkret   <- Return.annualized(tail(strat[,"Return"],x), scale = 252)
  actret   <- Return.annualized.excess(tail(strat[,"strat"],x), tail(strat[,"Return"],x), scale = 252, geometric = FALSE)
  output   <- t(data.frame(StrategyReturn = stratret,
                           BenchmarkReturn = bmkret,
                           ActiveReturn = actret))
  output
})

retdf <- data.frame(do.call("cbind", retdf))
names(retdf) <- c("1-Year", "3-Year", "5-Year", "7-Year", "10-Year", "15-Year", "20-Year")
rownames(retdf) <- c("Strategy Return", "Benchmark Return", "Active Return")

kable(retdf,
      digits = 4,
      format = "html",
      booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## Rolling Sharpe Ratio

rollsr <- lapply(253:nrow(strat), FUN = function(x){
  dte <- index(strat)[[x]]
  sr  <- SharpeRatio.annualized(strat[,"strat"][(x-252):x], scale = 252)
  output <- data.frame(Date = dte,
                       SharpeRatio = sr)
  output
})

rollsr <- do.call("rbind", rollsr)
rollsr <- rollsr[which(rollsr$Date == unique(rollsr$Date)),]
rownames(rollsr) <- rollsr$Date
rollsr <- tail(rollsr, 5000)


ggplot(data = rollsr, aes(x = Date, y = strat))+
  geom_hline(yintercept = 0, color = "black", alpha = 0.5)+
  geom_line(color = "red", size = 1) + 
  ggtitle("Rolling 1-Year SharpeRatio") +
  xlab("Date") +
  ylab("Sharpe Ratio")

## Consistency Chart

subx <- mstrat

ret <- lapply(36:nrow(mstrat), FUN = function(x){
  subdf <- tail(subx[1:x,],36)
  ret <- Return.annualized(R = subdf[,"strat"], scale = 12)
  bmk <- Return.annualized(R = subdf[,"Return"], scale = 12)
  ret <- as.xts(merge(ret,bmk), order.by=tail(index(subdf),1))
  ret
})
rets <- do.call("rbind", ret)
rets <- data.frame(rets)
rets$Date <- as.Date(row.names(rets))
rets$Date <- c(1:nrow(rets))   

rets$pointcol <- ifelse(rets$strat>rets$Return,"black","red")
ggplot(data = rets, aes(x=Return, y = strat, alpha = Date)) +
  geom_point(size=1, color = rets$pointcol)+
  scale_alpha_continuous(range = c(0.1,1), guide = "none")+
  geom_vline(xintercept = 0, color = "grey", alpha = 0.5)+
  geom_hline(yintercept = 0, color = "grey", alpha = 0.5)+
  geom_abline(intercept = 0, slope = 1, color = "grey", alpha = 0.5)+
  ggtitle("Rolling 36 Month Returns")+
  xlab("Benchmark Return (%)")+
  ylab("Managed Return (%)")+
  theme_economist_white()+
  theme(plot.title = element_text(hjust = 0.5, family = "Times", face="italic"))



## Rolling 3-Year Active Returns

roll <- mstrat
rollact <- lapply(60:nrow(roll), FUN = function(x){
  dte <- index(roll)[[x]]
  stratreturn  <- Return.annualized(tail(roll[1:x,"strat"],60), scale = 12, geometric = TRUE)
  bmkreturn    <- Return.annualized(tail(roll[1:x,"Return"],60), scale = 12, geometric = TRUE)
  activereturn <- stratreturn - bmkreturn
  
  output <- data.frame(Date = dte,
                       ActiveReturn = activereturn)
  output
})

rollact <- do.call("rbind", rollact)
rownames(rollact) <- rollact$Date

ggplot(data = rollact, aes(x = Date, y = strat))+
  geom_hline(yintercept = 0, color = "black", alpha = 0.5)+
  geom_line(color = "red", size = 1)

## Chart Drawdowns
strat <- asset[,"strat"]
chart.Drawdown(strat)


## Drawdown Tables
strat <- asset[,"strat"]
kable(table.Drawdowns(strat, top = 5, digits = 4, geometric = TRUE),
      format = "html",
      booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))



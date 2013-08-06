QuoteBond <- list()
QuoteBond[['070010.IB']]$date=c(as.Date("2012-12-10"))
QuoteBond[['070010.IB']]$priceDirty=c(106.7395)
QuoteBond[['070010.IB']]$accruedInterest=c(1.5671)
QuoteBond[['070010.IB']]$priceClean=c(105.1724)
QuoteBond[['070010.IB']]$YTM=c(10)#c(3.1928)
QuoteBond[['070010.IB']]$duration=c(4.0728)
QuoteBond[['070010.IB']]$convexity=c(21.6808)
BondInfo = ResetToday(BondInfo,"GOV","2012-12-10",FALSE,FALSE,TRUE)
BondInfo = InitBondPrice(BondInfo,"GOV",QuoteBond)

ytms = c(rep(0,length(BondInfo$GOV$ISIN)))
names(ytms) = c(BondInfo$GOV$ISIN)
ytms[1]=3
prices=BondYTM2PRICE(BondInfo,"GOV",ytms)
prices
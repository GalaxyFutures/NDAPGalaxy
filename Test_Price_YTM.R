BondInfo = ResetToday(BondInfo,"GOV","2012-12-10",FALSE,FALSE,TRUE)
BondInfo = InitBondPrice(BondInfo,"GOV",QuoteBond)
ytms = c(rep(0,length(BondInfo$GOV$ISIN)))

names(ytms) = c(BondInfo$GOV$ISIN)

prices=BondYTM2PRICE(BondInfo,"GOV",ytms)

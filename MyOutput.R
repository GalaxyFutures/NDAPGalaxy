

BondInfo = ResetToday(BondInfo,"GOV","2012-11-01",FALSE,FALSE,TRUE)


BondInfo = AddTFInfo(BondInfo,"GOV",TFInfo)
BondInfo = InitBondPrice(BondInfo,"GOV",QuoteBond)
BondInfo = InitTFPrice(BondInfo,"GOV",QuoteTF)

BondInfo = CalculateExpectedTFPrice(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)
BondInfo = CalculateNetBasis(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)
BondInfo = CalculateIRR(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)

BondInfo = InitBPV(BondInfo,"GOV", QuoteBond) 
BondInfo = FindCTD(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)
BondInfo = CalculateBPVTF(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)

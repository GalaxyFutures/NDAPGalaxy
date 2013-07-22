

BondInfo = ResetToday(BondInfo,"GOV","2012-11-1",FALSE,FALSE,TRUE)

#长时间不调用mongodb会断开
#QuoteMoneyMarket = GetQuoteMoneyMarketFromMongoDb("R001","average")

BondInfo = AddTFInfo(BondInfo,"GOV",TFInfo)
BondInfo = InitBondPrice(BondInfo,"GOV",QuoteBond)
BondInfo = InitTFPrice(BondInfo,"GOV",QuoteTF)

BondInfo = CalculateExpectedTFPrice(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)
BondInfo = CalculateNetBasis(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)
BondInfo = CalculateIRR(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)

BondInfo = InitBPV(BondInfo,"GOV", QuoteBond) 
BondInfo = FindCTD(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)
BondInfo = CalculateBPVTF(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)

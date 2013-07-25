#日回购: R001,R003,R007,R014,R021
#月回购：R1M,R3M,R4M,R6M,R9M
#年回购: R1Y
InitMongoDb("221.133.243.54:3401","NDAPReader","Reader@Galaxy")
QuoteMoneyMarket = GetQuoteMoneyMarketFromMongoDb("R001","average")
#上面的代码只需要运行一次即可



BondInfo = ResetToday(BondInfo,"GOV","2012-9-17",FALSE,FALSE,TRUE)




BondInfo = AddTFInfo(BondInfo,"GOV",TFInfo)
BondInfo = InitBondPrice(BondInfo,"GOV",QuoteBond)
BondInfo = InitTFPrice(BondInfo,"GOV",QuoteTF)

BondInfo = CalculateExpectedTFPrice(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)
BondInfo = CalculateNetBasis(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)
BondInfo = CalculateIRR(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)

BondInfo = InitBPV(BondInfo,"GOV", QuoteBond) 
BondInfo = FindCTD(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)
BondInfo = CalculateBPVTF(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)

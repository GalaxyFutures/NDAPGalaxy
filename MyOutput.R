#日回购: R001,R003,R007,R014,R021
#月回购：R1M,R3M,R4M,R6M,R9M
#年回购: R1Y
InitMongoDb("221.133.243.54:3401","NDAPReader","Reader@Galaxy")
QuoteMoneyMarket = GetQuoteMoneyMarketFromMongoDb("R001","average")
#上面的代码只需要运行一次即可

BondInfo = InitGovBondInfo( DbBondInfo )
BondInfo = ResetToday(BondInfo,"GOV","2013-8-15",FALSE,FALSE,TRUE)

###################################################################################
#计算基本信息
BondInfo = AddTFInfo(BondInfo,"GOV",TFInfo)
BondInfo = InitBondPrice(BondInfo,"GOV",QuoteBond)
BondInfo = InitTFPrice(BondInfo,"GOV",QuoteTF)

#CF修改
#CF修改结束
BondInfo = CalculateExpectedTFPrice(BondInfo,"GOV",TFInfo,QuoteMoneyMarket)
  
BondInfo = CalculateNetBasis(BondInfo,"GOV")

#计算TFIRR
BondInfo = CalculateIRR(BondInfo,"GOV",TFInfo)



#计算期货价格反推的债券的价格
BondInfo = Calculate_BondPrice_from_TFPrice(BondInfo,"GOV",TFInfo,QuoteMoneyMarket)





#计算CTD（TFIRR方法）
BondInfo = FindCTD(BondInfo,"GOV")

#计算Bond的BPV
BondInfo = InitBPV(BondInfo,"GOV") 

#计算TF的BPV（调用CalculateExpectedTFPrice）
BondInfo = CalculateBPVTF(BondInfo,"GOV",TFInfo,QuoteMoneyMarket)





CalculateRepoRelated = function(bonddata,group,TFInfo,QuoteMoneyMarket_AllRepo,BondYTMBasis = 0,MoneyMarketBasis = 0)
{
  result <- list()
  
  for (i in 1:length(QuoteMoneyMarket_AllRepo))
  {
    QuoteMoneyMarket = list()
    QuoteMoneyMarket$R1M = c(QuoteMoneyMarket_AllRepo[i])
    QuoteMoneyMarket$date = c(bonddata[[group]]$TODAY)
    
    
    temp_bonddata = CalculateExpectedTFPrice(bonddata,group,TFInfo,QuoteMoneyMarket,BondYTMBasis,MoneyMarketBasis)
    temp_bonddata = CalculateNetBasis(temp_bonddata,group)
    temp_bonddata = CalculateBPVTF(temp_bonddata,group,TFInfo,QuoteMoneyMarket)
    
    result[[i]]=list()
    result[[i]]$PRICE = temp_bonddata[[group]]$PRICE
    result[[i]]$expectedTFPrice = temp_bonddata[[group]]$expectedTFPrice
    
    result[[i]]$netBasis = temp_bonddata[[group]]$netBasis
    
    result[[i]]$BPVTF_CTDslashCF = temp_bonddata[[group]]$BPVTF_CTDslashCF
    result[[i]]$BPVTF_YTM = temp_bonddata[[group]]$BPVTF_YTM
    result[[i]]$BPVTF_R1M = temp_bonddata[[group]]$BPVTF_R1M
    
  }
  
  result
    
}


QuoteMoneyMarket_AllRepo = c(3.5,4.0,2.5)

result = CalculateRepoRelated(BondInfo,"GOV",TFInfo,QuoteMoneyMarket_AllRepo)





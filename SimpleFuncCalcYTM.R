

#需要知道一组价格/ytm,（known_price/known_ytm）和久期，求price对应的ytm
func_calculate_ytm = function(known_price,known_ytm,dur,price)
{
  ytm_diff = (price-known_price)/known_price/dur*100
  ytm = known_ytm - ytm_diff
  ytm
}

Calculate_BondPrice_from_TFPrice = function(bonddata,group,tFPrice,tFInfo,quoteMoneyMarket)#tFInfo
{
  ##读入行情数据
  r = quoteMoneyMarket$R1M[which(quoteMoneyMarket$date == bonddata[[group]]$TODAY)]/100
  ##计算必要数据，在此函数前调用
  #bonddata = AddtFInfo(bonddata,group,tFInfo)
  #bonddata = InitBondPrice(bonddata,group,QuoteBond)
  #bonddata = InitTFPrice(bonddata,group,QuoteTF)
  
  FVcoupon = CalculateFVcoupon(bonddata,group,tFInfo,r)
  daysToDelivery = matrix(data = tFInfo$settlementDate - as.Date(bonddata[[group]]$TODAY),
                          nr = length(tFInfo$TFname),
                          nc = length(bonddata[[group]]$ISIN),
                          byrow = FALSE)
  
  bondPrice = (tFPrice * bonddata[[group]]$conversionFactor + FVcoupon + bonddata[[group]]$accruedInterest)/(1 + r*daysToDelivery/365) - 
    matrix(data = (bonddata[[group]]$ACCRUED),
           nr = length(tFInfo$TFname),
           nc = length(bonddata[[group]]$ISIN),
           byrow = TRUE)
  
  
  bondPrice[which(bonddata[[group]]$deliverable == FALSE)] = 0
  
  dimnames(bondPrice) = list(tFInfo$TFname,bonddata[[group]]$ISIN)
  
  bondPrice
}


#last_price_dirty最新价格（全价）
#last_price_clean最新价格（净价）
#last_ytm:YIELD
#dur:久期
#price_clean所求的净价

last_price_dirty=96.5714
last_price_clean=95.9647
last_ytm=4.15
dur=6.1482
price_clean=96.31705

func_calculate_ytm = function(couponRate,issueDate,endDate,freq,today,price)
{
  ytm = Bondprice2ytm(couponRate,issueDate,endDate,freq,today,price)[1]
  ytm = round(ytm/0.005)*0.005
  ytm
}

Calculate_BondPrice_from_TFPrice = function(bonddata,group,TFPrice,TFInfo,QuoteMoneyMarket)#TFInfo
{
  ##读入行情数据
  r = QuoteMoneyMarket$R1M[which(QuoteMoneyMarket$date == bonddata[[group]]$TODAY)]/100
  ##计算必要数据，在此函数前调用
  #bonddata = AddTFInfo(bonddata,group,TFInfo)
  #bonddata = InitBondPrice(bonddata,group,QuoteBond)
  #bonddata = InitTFPrice(bonddata,group,QuoteTF)
  
  FVcoupon = CalculateFVcoupon(bonddata,group,TFInfo,r)
  daysToDelivery = matrix(data = TFInfo$settlementDate - as.Date(bonddata[[group]]$TODAY),
                          nr = length(TFInfo$TFname),
                          nc = length(bonddata[[group]]$ISIN),
                          byrow = FALSE)
  
  bondPrice = (TFPrice * bonddata[[group]]$conversionFactor + FVcoupon + bonddata[[group]]$accruedInterest)/(1 + r*daysToDelivery/365) - 
    matrix(data = (bonddata[[group]]$ACCRUED),
           nr = length(TFInfo$TFname),
           nc = length(bonddata[[group]]$ISIN),
           byrow = TRUE)
  
  
  bondPrice[which(bonddata[[group]]$deliverable == FALSE)] = 0
  
  dimnames(bondPrice) = list(TFInfo$TFname,bonddata[[group]]$ISIN)
  
  bondPrice
}

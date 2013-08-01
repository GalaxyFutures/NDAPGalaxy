########################################################################################
###########           CTD切换                   #########################

#YtmShift = c(-100,-50,0,50,100)
#TFName = "TF1403"
#bonddata = 经过resetToday后的BondInfo
#如需重置today，需运行
#DbBondInfo = GetUsefulTreasureInfosFromMongo()#GetTreasureInfosFromMongo() 
#BondInfo = InitGovBondInfo( DbBondInfo )
#resetToday

CTDScenario = function(bonddata,group,QuoteBond,TFName,YtmShift)
{
  BasisDiff = matrix(0,nr = length(YtmShift),nc = length(bonddata[[group]]$ISIN))
  YtmShift = YtmShift/10000
  for(i in 1:length(YtmShift))
  {
    #BondInfo = ResetToday(BondInfo,"GOV","2013-7-15",FALSE,FALSE,TRUE)
    
    bonddata_temp = CalculateNetBasis(BondInfo,group,TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket,BondYTMBasis = YtmShift[i])
    netBasis = bonddata_temp[[group]]$netBasis
    netBasis[netBasis ==0] = 2000
    BasisDiff[i,] = netBasis[TFName,] - min(netBasis[TFName,])
  }
  
  BasisDiff = t(BasisDiff)
  BasisDiff = round(BasisDiff,3)
  dimnames(BasisDiff) = list(bonddata[[group]]$ISIN,YtmShift)
  
  BasisDiff = data.frame(cbind(COUPONRATE=bonddata[[group]]$COUPONRATE*100,MATURITYDATE=bonddata[[group]]$MATURITYDATE,PRICE=bonddata[[group]]$PRICE,YTM=bonddata[[group]]$YTM,BasisDiff),check.names = TRUE)
  BasisDiff = BasisDiff[order(BasisDiff[,7],decreasing=FALSE),]
  BasisDiff$MATURITYDATE = as.Date(BasisDiff$MATURITYDATE)
  BasisDiff

}
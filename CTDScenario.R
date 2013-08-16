########################################################################################
###########           CTD切换                   #########################

#ytmShift = c(-100,-50,0,50,100)
#tfName = "TF1403"
#bonddata = 经过resetToday后的BondInfo
#如需重置today，需运行
#DbBondInfo = GetUsefulTreasureInfosFromMongo()#GetTreasureInfosFromMongo() 
#BondInfo = InitGovBondInfo( DbBondInfo )
#resetToday

CTDScenario = function(bonddata,group,TFInfo,QuoteMoneyMarket,tfName,ytmShift)
{
  BasisDiff = matrix(0,nr = length(ytmShift),nc = length(bonddata[[group]]$ISIN))
  ytmShift = ytmShift/10000
  for(i in 1:length(ytmShift))
  {
    bonddata = CalculateExpectedTFPrice(bonddata,group,TFInfo,QuoteMoneyMarket,ytmShift[i])
    bonddata_temp = CalculateNetBasis(bonddata,group)
    netBasis = bonddata_temp[[group]]$netBasis
    netBasis[netBasis ==0] = 2000
    BasisDiff[i,] = netBasis[tfName,] - min(netBasis[tfName,])
  }
  
  BasisDiff = t(BasisDiff)
  BasisDiff = round(BasisDiff,3)
  dimnames(BasisDiff) = list(bonddata[[group]]$ISIN,ytmShift)
  
  BasisDiff = data.frame(cbind(COUPONRATE=bonddata[[group]]$COUPONRATE*100,MATURITYDATE=bonddata[[group]]$MATURITYDATE,PRICE=bonddata[[group]]$PRICE,YTM=bonddata[[group]]$YTM,BasisDiff),check.names = TRUE)
  BasisDiff = BasisDiff[order(BasisDiff[,7],decreasing=FALSE),]
  BasisDiff$MATURITYDATE = as.Date(BasisDiff$MATURITYDATE)
  BasisDiff

}
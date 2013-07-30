###############################################################################################
##    BondInfo里的所有债券进行PRICE和YTM的互算，但是不修改BondInfo里面的值，只返回YTM或PRICE的值
##    要求BondInfo已经经过InitBondPrice函数调用                      #####################


BondYTM2PRICE = function(bondinfo,group,YTM)
{
  YTM = YTM / 100
  cf_p = create_cashflows_matrix(bondinfo[[group]])
  m_p = create_maturities_matrix_China(bondinfo[[group]])
  
  PRICE = bond_pricesClean_China(cf_p, m_p, YTM, bondinfo[[group]]$FREQUENCY)
  
  ##设置数据精度
  PRICE = round(PRICE,4)
  PRICE
}


BondPRICE2YTM = function(bondinfo,group,PRICE)
{  
  cf_p = create_cashflows_matrix(bondinfo[[group]])
  cf_p = rbind(-PRICE-bondinfo[[group]]$ACCRUED,cf_p)
  m_p = create_maturities_matrix_China(bondinfo[[group]])
  m_p = rbind(0,m_p)    
  bondyields = bond_yields_China(cf_p,m_p)
  YTM = t(bondyields[, 2])*100
  for (i in 1:length(bondinfo[[group]]$ISIN))
  {
    YTM[i]=YTM[i]*bondinfo[[group]]$FREQUENCY[i]
  }
  YTM=round(YTM,4)
  YTM
}
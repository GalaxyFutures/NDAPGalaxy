###############################################################################################
BondYTM2DurationConvexity = function(CouponRate,IssueDate,EndDate,freq,TODAY,YTM) #CouponRate,YTM单位%
{
  price = BondYTM2PRICE(CouponRate,IssueDate,EndDate,freq,TODAY,YTM)
  price_add = BondYTM2PRICE(CouponRate,IssueDate,EndDate,freq,TODAY,YTM+0.01)
  price_subtract = BondYTM2PRICE(CouponRate,IssueDate,EndDate,freq,TODAY,YTM-0.01)
  Duration = (price_subtract - price_add)/price/0.02*100
  #Convexity
  Duration_add = (price - BondYTM2PRICE(CouponRate,IssueDate,EndDate,freq,TODAY,YTM+0.02))/price_add/0.02*100
  Duration_subtract = (BondYTM2PRICE(CouponRate,IssueDate,EndDate,freq,TODAY,YTM-0.02) - price)/price_subtract/0.02*100
  Convexity = Duration * Duration - (Duration_add-Duration_subtract)/0.0002
  result = c(Duration,Convexity)
  result
}

BondYTM2PRICE = function(CouponRate,IssueDate,EndDate,freq,TODAY,YTM) #CouponRate,YTM单位%
{
  YTM = YTM / 100
  IssueDate = as.Date(IssueDate)
  EndDate = as.Date(EndDate)
  TODAY = as.Date(TODAY)
  
  if (freq == 1)
  {
    i = 1  
    nextPaymentDate = IssueDate
    while (nextPaymentDate<TODAY)
    {
      nextPaymentDate = seq(IssueDate,by ="1 year",length = i+1)[i+1]
      i = i+1
    }
    lastPaymentDate = seq(nextPaymentDate,by ="-1 year",length = 2)[2]
    
    i = 1
    temp_date = nextPaymentDate
    while (temp_date<EndDate)
    {
      temp_date = seq(temp_date,by ="1 year",length = 2)[2]
      i = i + 1
    }
    
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(TODAY), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = i)
    cf_p = rep(CouponRate/freq,i)
    cf_p[i]=100+CouponRate/freq
    ACCRUED = (CouponRate/freq) * (1 - m_p[1])
    
    cf_p = matrix(cf_p,byrow = TRUE)
    m_p = matrix(m_p,byrow = TRUE)
    
    PRICE = bond_pricesDirty_China(cf_p, m_p, YTM, freq) - ACCRUED
  }
  
  if (freq == 2)
  {
    i = 1   
    nextPaymentDate = IssueDate
    while (nextPaymentDate<TODAY)
    {
      nextPaymentDate = seq(IssueDate,by ="6 month",length = i+1)[i+1]
      i = i+1
    }
    lastPaymentDate = seq(nextPaymentDate,by ="-6 month",length = 2)[2]
    
    i = 1
    temp_date = nextPaymentDate
    while (temp_date<EndDate)
    {
      temp_date = seq(temp_date,by ="6 month",length = 2)[2]
      i = i + 1
    }
    
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(TODAY), units = "days"))
               /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
               by = 1, length = i)
    cf_p = rep(CouponRate/freq,i)
    cf_p[i]=100+CouponRate/freq
    ACCRUED = (CouponRate/freq) * (1 - m_p[1])
    
    cf_p = matrix(cf_p,byrow = TRUE)
    m_p = matrix(m_p,byrow = TRUE)
    
    PRICE = bond_pricesDirty_China(cf_p, m_p, YTM, freq) - ACCRUED
  }
  
  if (freq == -1)
  {
    nextPaymentDate = EndDate
    lastPaymentDate = IssueDate
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(TODAY), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = 1)
    cf_p = rep(0,1)
    cf_p[1]=100+CouponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days"))
    ACCRUED = CouponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days")) * (1 - m_p[1])
  
  
    cf_p = matrix(cf_p,byrow = TRUE)
    m_p = matrix(m_p,byrow = TRUE)
  
    PRICE = bond_pricesDirty_China(cf_p, m_p, YTM, 1) - ACCRUED
  }
  
  
  ##设置数据精度
  PRICE = round(PRICE,4)
  PRICE
}


BondPRICE2YTM = function(CouponRate,IssueDate,EndDate,freq,TODAY,PRICE)
{  
  IssueDate = as.Date(IssueDate)
  EndDate = as.Date(EndDate)
  TODAY = as.Date(TODAY)
  
  if (freq == 1)
  {
    i = 1  
    nextPaymentDate = IssueDate
    while (nextPaymentDate<TODAY)
    {
      nextPaymentDate = seq(IssueDate,by ="1 year",length = i+1)[i+1]
      i = i+1
    }
    lastPaymentDate = seq(nextPaymentDate,by ="-1 year",length = 2)[2]
    
    i = 1
    temp_date = nextPaymentDate
    while (temp_date<EndDate)
    {
      temp_date = seq(temp_date,by ="1 year",length = 2)[2]
      i = i + 1
    }
    
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(TODAY), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = i)
    cf_p = rep(CouponRate/freq,i)
    cf_p[i]=100+CouponRate/freq
    ACCRUED = (CouponRate/freq) * (1 - m_p[1])
  }
  
  if (freq == 2)
  {
    i = 1   
    nextPaymentDate = IssueDate
    while (nextPaymentDate<TODAY)
    {
      nextPaymentDate = seq(IssueDate,by ="6 month",length = i+1)[i+1]
      i = i+1
    }
    lastPaymentDate = seq(nextPaymentDate,by ="-6 month",length = 2)[2]
    
    i = 1
    temp_date = nextPaymentDate
    while (temp_date<EndDate)
    {
      temp_date = seq(temp_date,by ="6 month",length = 2)[2]
      i = i + 1
    }
    
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(TODAY), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = i)
    cf_p = rep(CouponRate/freq,i)
    cf_p[i]=100+CouponRate/freq
    ACCRUED = (CouponRate/freq) * (1 - m_p[1])
  }
  
  if (freq == -1)
  {
    nextPaymentDate = EndDate
    lastPaymentDate = IssueDate
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(TODAY), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = 1)
    cf_p = rep(0,1)
    cf_p[1]=100+CouponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days"))
    ACCRUED = CouponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days")) * (1 - m_p[1])
  }   
  
  cf_p = matrix(cf_p,byrow = TRUE)
  m_p = matrix(m_p,byrow = TRUE)
  cf_p = rbind(-PRICE-ACCRUED,cf_p)
  m_p = rbind(0,m_p)
  
  bondyields = bond_yields_China(cf_p,m_p)
  YTM = bondyields[,2]*100*freq

  YTM=round(YTM,4)
  YTM
}




























###############################################################################################
Bondytm2DurationConvexity = function(couponRate,issueDate,endDate,freq,today,ytm) #couponRate,ytm单位%
{
  accrued = Bondytm2price(couponRate,issueDate,endDate,freq,today,ytm)[2]
  price = Bondytm2price(couponRate,issueDate,endDate,freq,today,ytm)[1] + accrued
  price_add = Bondytm2price(couponRate,issueDate,endDate,freq,today,ytm+0.01)[1] + accrued
  price_subtract = Bondytm2price(couponRate,issueDate,endDate,freq,today,ytm-0.01)[1] + accrued
  Duration = (price_subtract - price_add)/price/0.02*100
  #Convexity
  Duration_add = (price - Bondytm2price(couponRate,issueDate,endDate,freq,today,ytm+0.02)[1] - accrued)/price_add/0.02*100
  Duration_subtract = (Bondytm2price(couponRate,issueDate,endDate,freq,today,ytm-0.02)[1] + accrued - price)/price_subtract/0.02*100
  Convexity = Duration * Duration - (Duration_add-Duration_subtract)/0.0002
  result = c(Duration,Convexity)
  result
}

Bondytm2price = function(couponRate,issueDate,endDate,freq,today,ytm) #couponRate,ytm单位%
{
  ytm = ytm / 100
  issueDate = as.Date(issueDate)
  endDate = as.Date(endDate)
  today = as.Date(today)
  
  if (freq == 1)
  {
    i = 1  
    nextPaymentDate = issueDate
    while (nextPaymentDate<=today)
    {
      nextPaymentDate = seq(issueDate,by ="1 year",length = i+1)[i+1]
      i = i+1
    }
    lastPaymentDate = seq(nextPaymentDate,by ="-1 year",length = 2)[2]
    
    i = 1
    temp_date = nextPaymentDate
    while (temp_date<endDate)
    {
      temp_date = seq(temp_date,by ="1 year",length = 2)[2]
      i = i + 1
    }
    
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(today), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = i)
    cf_p = rep(couponRate/freq,i)
    cf_p[i]=100+couponRate/freq
    
    #如果当天是付息日则应计利息定为0
    ACCRUED = 0
    if(m_p[1]!=0) ACCRUED = (couponRate/freq) * (1 - m_p[1])
    
    cf_p = matrix(cf_p,byrow = TRUE)
    m_p = matrix(m_p,byrow = TRUE)
    
    price = bond_pricesDirty_China(cf_p, m_p, ytm, freq) - ACCRUED
  }
  
  if (freq == 2)
  {
    i = 1   
    nextPaymentDate = issueDate
    while (nextPaymentDate<=today)
    {
      nextPaymentDate = seq(issueDate,by ="6 month",length = i+1)[i+1]
      i = i+1
    }
    lastPaymentDate = seq(nextPaymentDate,by ="-6 month",length = 2)[2]
    
    i = 1
    temp_date = nextPaymentDate
    while (temp_date<endDate)
    {
      temp_date = seq(temp_date,by ="6 month",length = 2)[2]
      i = i + 1
    }
    
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(today), units = "days"))
               /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
               by = 1, length = i)
    cf_p = rep(couponRate/freq,i)
    cf_p[i]=100+couponRate/freq
    
    #如果当天是付息日则应计利息定为0
    ACCRUED = 0
    if(m_p[1]!=0) ACCRUED = (couponRate/freq) * (1 - m_p[1])
    
    cf_p = matrix(cf_p,byrow = TRUE)
    m_p = matrix(m_p,byrow = TRUE)
    
    price = bond_pricesDirty_China(cf_p, m_p, ytm, freq) - ACCRUED
  }
  
  if (freq == -1)
  {
    nextPaymentDate = endDate
    lastPaymentDate = issueDate
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(today), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = 1)
    cf_p = rep(0,1)
    cf_p[1]=100+couponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days"))
    
    #如果当天是付息日则应计利息定为0
    ACCRUED = 0
    if(m_p[1]!=0) ACCRUED = couponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days")) * (1 - m_p[1])
  
    cf_p = matrix(cf_p,byrow = TRUE)
    m_p = matrix(m_p,byrow = TRUE)
  
    price = bond_pricesDirty_China(cf_p, m_p, ytm, 1) - ACCRUED
  }
  
  
  ##设置数据精度
  #price = round(price,4)
  result = c(price,ACCRUED)
  result
}


Bondprice2ytm = function(couponRate,issueDate,endDate,freq,today,price)
{  
  issueDate = as.Date(issueDate)
  endDate = as.Date(endDate)
  today = as.Date(today)
  
  if (freq == 1)
  {
    i = 1  
    nextPaymentDate = issueDate
    while (nextPaymentDate<=today)
    {
      nextPaymentDate = seq(issueDate,by ="1 year",length = i+1)[i+1]
      i = i+1
    }
    lastPaymentDate = seq(nextPaymentDate,by ="-1 year",length = 2)[2]
    
    i = 1
    temp_date = nextPaymentDate
    while (temp_date<endDate)
    {
      temp_date = seq(temp_date,by ="1 year",length = 2)[2]
      i = i + 1
    }
    
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(today), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = i)
    cf_p = rep(couponRate/freq,i)
    cf_p[i]=100+couponRate/freq
    #如果当天是付息日则应计利息定为0
    ACCRUED = 0
    if(m_p[1]!=0) ACCRUED = (couponRate/freq) * (1 - m_p[1])
  }
  
  if (freq == 2)
  {
    i = 1   
    nextPaymentDate = issueDate
    while (nextPaymentDate<=today)
    {
      nextPaymentDate = seq(issueDate,by ="6 month",length = i+1)[i+1]
      i = i+1
    }
    lastPaymentDate = seq(nextPaymentDate,by ="-6 month",length = 2)[2]
    
    i = 1
    temp_date = nextPaymentDate
    while (temp_date<endDate)
    {
      temp_date = seq(temp_date,by ="6 month",length = 2)[2]
      i = i + 1
    }
    
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(today), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = i)
    cf_p = rep(couponRate/freq,i)
    cf_p[i]=100+couponRate/freq
    #如果当天是付息日则应计利息定为0
    ACCRUED = 0
    if(m_p[1]!=0) ACCRUED = (couponRate/freq) * (1 - m_p[1])
  }
  
  if (freq == -1)
  {
    nextPaymentDate = endDate
    lastPaymentDate = issueDate
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(today), units = "days"))
              /as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate), units = "days")),
              by = 1, length = 1)
    cf_p = rep(0,1)
    cf_p[1]=100+couponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days"))
    #如果当天是付息日则应计利息定为0
    ACCRUED = 0
    if(m_p[1]!=0) ACCRUED = couponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days")) * (1 - m_p[1])
  }   
  
  cf_p = matrix(cf_p,byrow = TRUE)
  m_p = matrix(m_p,byrow = TRUE)
  cf_p = rbind(-price-ACCRUED,cf_p)
  m_p = rbind(0,m_p)
  
  bondyields = bond_yields_China(cf_p,m_p)
  ytm = bondyields[,2]*100*freq

  #ytm=round(ytm,4)
  result = c(ytm,ACCRUED)
  result
}







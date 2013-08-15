


#Year = c(1,2,3,5,7,10,30,50)  1Y,2Y,5Y,10Y,30Y,50Y
#Yield = c(3.5,3.6,3.7,3.8,3.9,4.0,4.1,4.2)
#yc = rbind(Year,Yield) 


outputTFYC2Duration = function(r,settlementDate,couponRate,issueDate,endDate,freq,today,yc_init)
{
  tfDaysToMaturity = as.numeric(difftime(as.Date(settlementDate), as.Date(today), units = "days"))
  KeyRateDuration = cbind(c(0,0),yc_init)
  
  KeyRateDuration[2,1]= tfDaysToMaturity/(365+r/100*tfDaysToMaturity)
  
  for (j in 2:ncol(KeyRateDuration))
  {
    KeyRateDuration[2,j]= BondYC2Duration(couponRate,issueDate,endDate,freq,today,yc_init,KeyRateDuration[1,j],interval=1)[1]
  }
  KeyRateDuration
}

outputBondYC2Duration = function(couponRate,issueDate,endDate,freq,today,yc_init)
{
  #Ys = c(1,2,3,5,7,10,30,50)
  KeyRateDuration = yc_init
  for (j in 1:ncol(KeyRateDuration))
  {
    KeyRateDuration[2,j]= BondYC2Duration(couponRate,issueDate,endDate,freq,today,yc_init,KeyRateDuration[1,j],interval=1)[1]
  }
  KeyRateDuration
}


#给定初始收益率曲线yc_init和债券基本信息，计算在ratePoint点的KeyRateDuration
BondYC2Duration = function(couponRate,issueDate,endDate,freq,today,yc_init,ratePoint,interval=2) #interval = +- 2Yrs,ratePoint +- 10 bp
{
  issueDate = as.Date(issueDate)
  endDate = as.Date(endDate)
  today = as.Date(today)
  
  yc_init = cbind(c(0,yc_init[2,1]-yc_init[1,1]*(yc_init[2,2]-yc_init[2,1])/(yc_init[1,2]-yc_init[1,1])),yc_init)
  YCfunc_init <- approxfun(yc_init[1,], yc_init[2,])
  
  #构造ratePoint点收益率上移10bp的收益率曲线,假定在t=0的收益率不变
  yc_up = yc_init
  j = 1
  while (j <=ncol(yc_up))
  {
    if(yc_up[1,j]<=(ratePoint + interval) & yc_up[1,j]>=(ratePoint - interval))
      yc_up = yc_up[,-j]
    else
    j = j + 1
  }
  
  yc_up <- cbind(c(0,YCfunc_init(0)),c(ratePoint,YCfunc_init(ratePoint)+0.1),c(ratePoint+interval,YCfunc_init(ratePoint+interval)),c(ratePoint-interval,YCfunc_init(ratePoint-interval)),yc_up)
  YCfunc_up <- approxfun(yc_up[1,], yc_up[2,])
  
  #构造ratePoint点收益率下移10bp的收益率曲线,假定在t=0的收益率不变
  yc_down = yc_init
  j = 1
  while (j <=ncol(yc_down))
  {
    if(yc_down[1,j]<=(ratePoint + interval) & yc_down[1,j]>=(ratePoint - interval))
      yc_down = yc_down[,-j]
    else
      j = j + 1
  }
  
  yc_down <- cbind(c(0,YCfunc_init(0)),c(ratePoint,YCfunc_init(ratePoint)-0.1),c(ratePoint+interval,YCfunc_init(ratePoint+interval)),c(ratePoint-interval,YCfunc_init(ratePoint-interval)),yc_down)
  YCfunc_down <- approxfun(yc_down[1,], yc_down[2,])
  
  
  
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
    m_p = matrix(m_p,byrow = TRUE) / freq
    
    #计算初始价格
    y_init = YCfunc_init(m_p)
    y_init = matrix(y_init,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_init)^m_p
    price_init = apply(d,2,"sum") #全价
    
    #计算up的价格
    y_up = YCfunc_up(m_p)
    y_up = matrix(y_up,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_up)^m_p
    price_up = apply(d,2,"sum") #全价
      
    #计算down的价格
    y_down = YCfunc_down(m_p)
    y_down = matrix(y_down,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_down)^m_p
    price_down = apply(d,2,"sum") #全价
    
    KeyRateDuration = (price_down - price_up)/price_init/0.002

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
    m_p = matrix(m_p,byrow = TRUE) / freq
    
    #计算初始价格
    y_init = YCfunc_init(m_p)
    y_init = matrix(y_init,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_init)^m_p
    price_init = apply(d,2,"sum") #全价
    
    #计算up的价格
    y_up = YCfunc_up(m_p)
    y_up = matrix(y_up,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_up)^m_p
    price_up = apply(d,2,"sum") #全价
    
    #计算down的价格
    y_down = YCfunc_down(m_p)
    y_down = matrix(y_down,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_down)^m_p
    price_down = apply(d,2,"sum") #全价
    
    KeyRateDuration = (price_down - price_up)/price_init/0.002
    

  }
  
  if (freq == -1)
  {
    nextPaymentDate = endDate
    lastPaymentDate = issueDate
    m_p = seq(as.numeric(difftime(as.Date(nextPaymentDate), as.Date(today), units = "days"))/365, by = 1, length = 1)
    cf_p = rep(0,1)
    cf_p[1]=100+couponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days"))
    
    #如果当天是付息日则应计利息定为0
    ACCRUED = 0
    if(m_p[1]!=0) ACCRUED = couponRate/365*as.numeric(difftime(as.Date(nextPaymentDate), as.Date(lastPaymentDate),units = "days")) * (1 - m_p[1])
    
    cf_p = matrix(cf_p,byrow = TRUE)
    m_p = matrix(m_p,byrow = TRUE)
    
    #计算初始价格
    y_init = YCfunc_init(m_p)
    y_init = matrix(y_init,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_init)^m_p
    price_init = apply(d,2,"sum") #全价
    
    #计算up的价格
    y_up = YCfunc_up(m_p)
    y_up = matrix(y_up,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_up)^m_p
    price_up = apply(d,2,"sum") #全价
    
    #计算down的价格
    y_down = YCfunc_down(m_p)
    y_down = matrix(y_down,byrow = TRUE) / 100
    
    d <- cf_p / (1+y_down)^m_p
    price_down = apply(d,2,"sum") #全价
    
    KeyRateDuration = (price_down - price_up)/price_init/0.002
  }
  
  result = c(KeyRateDuration,price_init)
  result
  
}


############################################  test  #############################################
#couponRate,YC单位%
#YC为收益率曲线
Year = c(1/12,3/12,6/12,1,2,5,10,30,50)
Yield = c(3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4.0)
YC = rbind(Year,Yield) 
couponRate = 3
issueDate = "2007/7/1"
endDate = "2017/7/1"
freq = 1
today = "2013/7/1"
yc_init = YC
ratePoint = 2

BondYC2Duration(couponRate,issueDate,endDate,freq,today,yc_init,ratePoint,interval=2)
Bondytm2DurationConvexity(couponRate,issueDate,endDate,freq,today,ytm = 3.497)[1]
BondYC2Duration(couponRate,issueDate,endDate,freq,today,yc_init,ratePoint,interval=5)
  
  
  

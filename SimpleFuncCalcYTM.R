

#需要知道一组价格/ytm,（known_price/known_ytm）和久期，求price对应的ytm
func_calculate_ytm = function(known_price,known_ytm,dur,price)
{
  ytm_diff = (price-known_price)/known_price/dur*100
  ytm = known_ytm - ytm_diff
  ytm
}

#从债券基本信息计算ytm
func_calculate_ytm_bis = function(couponRate,issueDate,endDate,freq,today,price)
{
  
  ytm = Bondprice2ytm(couponRate,issueDate,endDate,freq,today,price)[1]
  ytm = round(ytm/0.005)*0.005
  ytm
}



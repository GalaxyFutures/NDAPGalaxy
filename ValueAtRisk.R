



CalculateVaR = function(priceVector,quantileLevel,currentPrice,historicMethod = TRUE)
{
  length = length(priceVector)
  returnVector = priceVector[-1]/priceVector[-100] -1
  quantle = quantile(returnVector,quantileLevel)
  VaR = -1 * quantle * currentPrice
  VaR
}

#####################################################################################################
#x为收益率向量，price为价格向量，currentPrice为120，quantileLevel为1%，默认使用历史模拟法：historicMethod = TRUE
x <- rnorm(100)
price = rep(100,100)
for (i in 2:100)
  price[i]=price[i-1]*(1+x[i])


VaR = CalculateVaR(price,0.01,120,TRUE)
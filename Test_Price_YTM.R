

###########################################################################################################
#已知ytm，求price，duration，convexity
#couponRate,ytm单位%
#求price用函数Bondytm2price
#price = 99.99278

couponRate = 3
issueDate = "2010/5/1"
endDate = "2014/5/1"
freq = 1
today = "2013/7/15"
ytm = 3
price = Bondytm2price(couponRate,issueDate,endDate,freq,today,ytm)[1]
print(price)
#求Duration和Convexity用函数Bondytm2DurationConvexity
Duration = Bondytm2DurationConvexity(couponRate,issueDate,endDate,freq,today,ytm)[1]
Convexity = Bondytm2DurationConvexity(couponRate,issueDate,endDate,freq,today,ytm)[2]
print(Duration)
print(Convexity)

###########################################################################################################
#已知price，求ytm，duration，convexity
#couponRate,ytm单位%
#求ytm用函数Bondprice2ytm

price = 99.99278
ytm = Bondprice2ytm(couponRate,issueDate,endDate,freq,today,price)
print(ytm)
#求Duration和Convexity用函数Bondytm2DurationConvexity,上面求出的ytm作为参数
Duration = Bondytm2DurationConvexity(couponRate,issueDate,endDate,freq,today,ytm)[1]
Convexity = Bondytm2DurationConvexity(couponRate,issueDate,endDate,freq,today,ytm)[2]
print(Duration)
print(Convexity)




couponRate = 4.69
issueDate = "2001/06/06"
endDate = "2016/06/06"
freq = 2
today = "2013/04/10"
price = 150
result = Bondprice2ytm(couponRate,issueDate,endDate,freq,today,price)





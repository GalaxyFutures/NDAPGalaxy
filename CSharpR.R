#1970年的Tick(c#)
g_lTicksOf1970 = 621355968000000000


#把C#中DateTime的Ticks转换成时间
GetDateFromTicks = function( arg_ticks)
{
  seconds = arg_ticks/10000000   
  as.POSIXct(seconds, origin = "01-01-01 00:00:00")
}

#arg_datetime必须是POSIXlt类型
# date = as.Date("2010-01-01")
# dateTime = as.POSIXlt(date)
GetTicksFromDateTime = function( arg_datetime)
{
  #1970年开始的秒数
  dtime =as.double(arg_datetime) + 8*60*60
  dtime*10000000 + g_lTicksOf1970  
}

GetDate = function( arg_seconds )
{
  #注意这里时间是+16,无奈之举,否则时间有错误,时差问题
  #数据库中时间是当地时间，读取出后需要+8才能使as.POSIXct显示正确的CTS时间
  #但是as.Date会根据时差，自动转成GMT时间,也就是自动减去8
  #所以必须+16.......我怎么觉得有点绕进去了 
  as.Date(as.POSIXct(arg_seconds, origin = "1970-01-01 16:00:00"))
}

GetYM = function( arg_dates)
{
  p = as.POSIXlt(arg_dates)
  
  as.Date(ISOdate(p$year+1900,p$mon+1,1))
}


SetQuoteMoneyMarket = function(arg_date, arg_dNewData, arg_quotemoneymarket)
{
  i = which(arg_quotemoneymarket$date == arg_date)  
  arg_quotemoneymarket$R1M[[i]]=arg_dNewData
  arg_quotemoneymarket
}



SetTFData = function(arg_TFName, arg_date,arg_strColumnName, arg_dNewData, arg_QuoteTF)
{
  i = which( arg_QuoteTF[[arg_TFName]]$date == arg_date)  
  arg_QuoteTF[[arg_TFName]][[arg_strColumnName]][[i]]=arg_dNewData
  arg_QuoteTF
}
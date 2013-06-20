#把C#中DateTime的Ticks转换成时间
GetDateFromTicks = function( arg_ticks)
{
  seconds = arg_ticks/10000000   
  as.POSIXct(seconds, origin = "01-01-01 00:00:00")
}


GetDate = function( arg_seconds )
{
  #注意这里时间是+16,无奈之举,否则时间有错误,时差问题
  #数据库中时间是当地时间，读取出后需要+8才能使as.POSIXct显示正确的CTS时间
  #但是as.Date会根据时差，自动转成GMT时间,也就是自动减去8
  #所以必须+16.......我怎么觉得有点绕进去了 
  as.Date(as.POSIXct(arg_seconds, origin = "1970-01-01 16:00:00"))
}
#把C#中DateTime的Ticks转换成时间
GetDateFromTicks = function( arg_ticks)
{
  seconds = arg_ticks/10000000   
  as.POSIXct(seconds, origin = "01-01-01 00:00:00")
}
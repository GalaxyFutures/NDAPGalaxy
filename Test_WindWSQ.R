w.start()

codes<-c("R001.IB","R007.IB","R1M.IB","R3M.IB")
OpenWSQ(codes)


cntG = length(gdata)
i=1
while(i<=cntG)
{
  item=gdata[[i]]
  item$Id
  item$RT_TIME
  item$RT_DATE
  item$RT_LAST
  i=i+1
}
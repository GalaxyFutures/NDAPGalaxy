
OpenWSQ = function(arg_codes)
{
   result<-w.wsq(arg_codes,"rt_date,rt_time,rt_last,",func=OnData)
}

gdata<-list()
OnData = function(arg_bonddata)
{ 
  data <<- arg_bonddata
  print(data)
  cntRow = length(data$Code)
  i = 1
  while(i<=cntRow)
  {
    item=list()        
    
    j=1  
    cntDataItem = length(data$Field)
    while(j<=cntDataItem)
    {
      item[[data$Field[[j]]]]=data$Data[[(i-1)*cntDataItem+j]]
      j=j+1
    }
    
    gdata[[data$Code[[i]]]]<<-item
    i= i+1
  }  
}




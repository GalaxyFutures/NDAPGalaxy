require(WindR)

i=0
RepoCallback = function(data)
{
  print("Data Count:")
  print(i)
  i = i+1
  print(data)
}

w_wsq_data<-w.wsq("R001.IB,R007.IB,R014.IB,R021.IB,R1M.IB,R1Y.IB,R2M.IB,R3M.IB,R4M.IB,R6M.IB,R9M.IB","rt_date,rt_time,rt_pre_close,rt_open,rt_last,rt_last_amt,rt_last_vol,rt_vol,rt_amt",func=RepoCallback)





init_data<-function(code)
{
  out<-list();
  out$RequestID=0;
  out$CODE=code;
  out$SEC_NAME=getnamebycode(code);
  out$RT_HIGH=NA;
  out$RT_LOW=NA
  out$RT_VWAP=NA
  out$RT_OPEN=NA
  out$RT_AMT=NA
  out$RT_SWING=NA
  out$RT_UPWARD_VOL=NA
  out$RT_DOWNWARD_VOL=NA
  out$RT_LAST_VOL=NA
  out$RT_VOL_RATIO=NA
  out$RT_VOL=NA
  out$RT_TURN=NA
  out$RT_BSIZE5=NA
  out$RT_BSIZE4=NA
  out$RT_BSIZE3=NA
  out$RT_BSIZE2=NA
  out$RT_BSIZE1 =NA
  out$RT_BID5 =NA
  out$RT_BID4 =NA
  out$RT_BID3 =NA
  out$RT_BID2=NA
  out$RT_BID1 =NA
  out$RT_ASK5 =NA
  out$RT_ASK4 =NA
  out$RT_ASK3 =NA
  out$RT_ASK2 =NA
  out$RT_ASK1 =NA
  out$RT_ASIZE5 =NA
  out$RT_ASIZE4 =NA
  out$RT_ASIZE3 =NA
  out$RT_ASIZE2 =NA
  out$RT_ASIZE1 =NA
  out$RT_LAST=NA
  out$RT_PRE_CLOSE =NA
  out$RT_CHG =NA
  out$RT_PCT_CHG=NA
  return(out)
}

#gStockData<-init_data()

wsqcallback<-function(data)
{
  if(data$ErrorCode!=0)
  {
    return()
  }
  #if(data$RequestID!=gStockData$RequestID)
  #{
  #  return();
  #}
  
  if(length(data$Code)!=1 || data$Code[[1]]!=gStockData$CODE)
  {
    return ();
  }
  
  updatename<-unlist(data$Field);
  gStockData[updatename]<<-unlist(data$Data)
  
  fname=c("RT_VOL","RT_UPWARD_VOL","RT_DOWNWARD_VOL","RT_LAST_VOL",
          "RT_ASIZE5","RT_ASIZE4","RT_ASIZE3","RT_ASIZE2","RT_ASIZE1",
          "RT_BSIZE5","RT_BSIZE4","RT_BSIZE3","RT_BSIZE2","RT_BSIZE1"
  );
  for(nn in updatename){
    
    if(any(nn==fname)>0){
      gStockData[[nn]]<<-gStockData[[nn]]/100
    }
  }
  
  if(any("RT_AMT"==updatename)>0)
    gStockData[["RT_AMT"]]<<-gStockData[["RT_AMT"]]/10000
  
  print(data)
}

startwsq<-function()
{
  data<-w.wsq(gStockData$CODE,"rt_high,rt_low,rt_vwap,rt_open,rt_amt,rt_swing,rt_upward_vol,rt_downward_vol,rt_last_vol,rt_vol_ratio,rt_vol,rt_turn,rt_bsize5,rt_bsize4,rt_bsize3,rt_bsize2,rt_bsize1,rt_bid5,rt_bid4,rt_bid3,rt_bid2,rt_bid1,rt_ask5,rt_ask4,rt_ask3,rt_ask2,rt_ask1,rt_asize5,rt_asize4,rt_asize3,rt_asize2,rt_asize1,rt_last,rt_pre_close,rt_chg,rt_pct_chg"
              ,func=wsqcallback)
  if(data$ErrorCode!=0)
  {
    print("call wsq error!")
    return()
  }
  
  gStockData$RequestID<<-data$RequestID;
  print(gStockData$RequestID)
}
stopwsq<-function()
{
  if(gStockData$RequestID[[1]]!=0)
    w.cancelRequest(gStockData$RequestID)
  gStockData$RequestID<<-0;
}






#code must be a stock
dotest<-function(code="600000.SH")
{
  w.start(showmenu=FALSE);
  testrealtime(code)
  print("stopwsq() to stop...")
}
dotest();
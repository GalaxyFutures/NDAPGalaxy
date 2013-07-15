

ndapdb<-0

InitMongoDb = function(arg_strAddress,arg_strUser,arg_strPass)
{
  ndapdb <<- mongo.create(host=arg_strAddress,username=arg_strUser,password=arg_strPass, db="NDAP")
}




#返回BondInfo
GetBondInfosFromMongo = function () 
{
  cols = GetColumnsFromMongo_Tool(ndapdb,"NDAP.BondVariety",F)
  
  cols = list( cols$'_id',cols$Name,cols$issuedate,cols$maturitydate,cols$couponrate,cols$frequency)
  names(cols)= c("code.IB","name" ,"issuedate","maturitydate","couponrate","frequency")
  cols    
}



#返回BondInfo
GetBondInfosFromMongo2 = function () 
{  
  rows = GetRowsFromMongo_Tool(ndapdb,"NDAP.BondVariety",F)  
  
  cntRow = length(rows)
  i = 1
  while(i<=cntRow)
  {
    if( rows[[i]]$frequency!=2 )
    {
      rows[[i]]<-NULL
      cntRow = cntRow-1;
      i=i-1
    }
    i= i+1
  }
  
  cols = Rows2Columns_Tool(rows)
  
  cols = list( cols$'_id',cols$Name,cols$issuedate,cols$maturitydate,cols$couponrate,cols$frequency)
  names(cols)= c("code.IB","name" ,"issuedate","maturitydate","couponrate","frequency")
  
  
  cols$issuedate= format( GetDate(cols$issuedate) ,format="%Y/%m/%d")
  cols$maturitydate= format(GetDate(cols$maturitydate),format="%Y/%m/%d")
  
  cols    
}

#仅返回国债BondInfo
GetTreasureInfosFromMongo = function () 
{  
  rows = GetRowsFromMongo_Tool(ndapdb,"NDAP.BondVariety",F)  
  
  cntRow = length(rows)
  i = 1
  while(i<=cntRow)
  {
    if( rows[[i]][[13]]!=1 )
    {
      rows[[i]]<-NULL
      cntRow = cntRow-1;
      i=i-1
    }
    i= i+1
  }
  
  cols = Rows2Columns_Tool(rows)
  
  cols = list( cols$'_id',cols$Name,cols$issuedate,cols$maturitydate,cols$couponrate,cols$frequency)
  names(cols)= c("code.IB","name" ,"issuedate","maturitydate","couponrate","frequency")
  
  
  cols$issuedate= format( GetDate(cols$issuedate) ,format="%Y/%m/%d")
  cols$maturitydate= format(GetDate(cols$maturitydate),format="%Y/%m/%d")
  
  cols    
}

#返回R1MR3M
GetR1MR3MFromMongo = function()
{
  strNS = paste("NDAP","R1MR3M",sep=".");
  cols = GetColumnsFromMongo_Tool(ndapdb,strNS,F)
  #date,open,high,low,close,average,volume,holding
  #R1MR3M = list(  format(as.POSIXct(cols$'_id', origin="1970-01-01 08:00:00"),format="%Y/%m/%d") ,cols$R1M,cols$R3M)
  R1MR3M = list(  format(GetDate(cols$'_id'),format="%Y/%m/%d") ,cols$R1M,cols$R3M)
  names(R1MR3M)=c("date","R1M","R3M")
  R1MR3M
}

#返回YTM R1M
GetYTMR1MFromMongo = function()
{
  strNS = paste("NDAP","R1M",sep=".");
  cols = GetColumnsFromMongo_Tool(ndapdb,strNS,F)
  #date,open,high,low,close,average,volume,holding
  #YTMR1M = list(  format(as.POSIXct(cols$'_id', origin="1970-01-01 08:00:00"),format="%Y/%m/%d") ,cols$YTMgov5year,cols$YTMgov3year,cols$r1m)
  YTMR1M = list(  format(GetDate(cols$'_id'),format="%Y/%m/%d") ,cols$YTMgov5year,cols$YTMgov3year,cols$r1m)
  names(YTMR1M)=c("date","YTMgov5year","YTMgov3year","R1M")
  YTMR1M
}


#返回TF日度数据
GetTFDailyDatasFromMongo = function(arg_strTF)
{ 
  strNS = paste("NDAP",arg_strTF,"Daily",sep=".");
  cols = GetColumnsFromMongo_Tool(ndapdb,strNS,F)
  #date,open,high,low,close,average,volume,holding 
  #datasDaily = list( as.Date(as.POSIXct(cols$'_id', origin="1970-01-01 16:00:00")),cols$open,cols$high,cols$low,cols$close,cols$average,cols$volume,cols$holding,cols$settle)
  datasDaily = list( GetDate(cols$'_id'),cols$open,cols$high,cols$low,cols$close,cols$average,cols$volume,cols$holding,cols$settle)
  names(datasDaily)=c("date","open","high","low","close","average","volume","holding","settle")
  datasDaily
}


GetBondDailyDatasFromMongo = function(arg_strBond)
{  
  #bondname="010214.IB"
  strNS = paste("NDAP.Bond",bondname,"Daily",sep="_");  
  cols = GetColumnsFromMongo_Tool(ndapdb,strNS,F)
  #cols$'_id' <- as.Date(GetDateFromTicks(cols$'_id'))
  cols$'_id' <- GetDate(cols$'_id')
  #date,open,high,low,close,average,volume,holding
  #datasDaily = data.frame( GetDateFromTicks(frm$X_id),frm$open,frm$high,frm$low,frm$close,frm$average,frm$volume,frm$holding)
  cols = list(cols$'_id',cols$'dirtyCsi',cols$'accruedInterestCsi',cols$'netCsi',cols$'yieldCsi',cols$'modiduraCsi',cols$'cnvxtyCsi')
  #date,priceDirty,accruedInterest,priceClean,YTM,duration,convexity
  names(cols)=c("date","priceDirty","accruedInterest","priceClean","YTM","duration","convexity")
  
  cols  
}
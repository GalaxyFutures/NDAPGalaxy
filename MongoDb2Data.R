

ndapdb

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
  
  
  cols$issuedate= format(as.POSIXct(cols$issuedate, origin="1970-01-01 08:00:00"),format="%Y/%m/%d")
  cols$maturitydate= format(as.POSIXct(cols$maturitydate, origin="1970-01-01 08:00:00"),format="%Y/%m/%d")
  
  cols    
}




#返回TF日度数据
GetTFDailyDatasFromMongo = function(arg_strTF)
{
  strNS = paste("NDAP",arg_strTF,"Day",sep=".");
  cols = GetColumnsFromMongo_Tool(ndapdb,strNS,F)
  #date,open,high,low,close,average,volume,holding
  datasDaily = list( GetDateFromTicks(cols$X_id),cols$open,cols$high,cols$low,cols$close,cols$average,cols$volume,cols$holding)
  names(datasDaily)=c("date","open","high","low","close","average","volume","holding")
  datasDaily
}


GetBondDailyDatasFromMongo = function(arg_strBond)
{
  
  strNS = paste("NDAP.Bond",bondname,"Daily",sep="_");  
  cols = GetColumnsFromMongo_Tool(ndapdb,strNS,F)
  cols$'_id' <- GetDateFromTicks(cols$'_id')
  #date,open,high,low,close,average,volume,holding
  #datasDaily = data.frame( GetDateFromTicks(frm$X_id),frm$open,frm$high,frm$low,frm$close,frm$average,frm$volume,frm$holding)
  cols = list(cols$'_id',cols$'dirtyCsi',cols$'accruedInterestCsi',cols$'netCsi',cols$'yieldCsi',cols$'modiduraCsi',cols$'cnvxtyCsi')
  #date,priceDirty,accruedInterest,priceClean,YTM,duration,convexity
  names(cols)=c("date","priceDirty","accruedInterest","priceClean","YTM","duration","convexity")
  
  cols  
}
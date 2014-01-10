

ndapdb<-0

InitMongoDb = function(arg_strAddress,arg_strUser,arg_strPass)
{
  ndapdb <<- mongo.create(host=arg_strAddress,username=arg_strUser,password=arg_strPass, db="NDAP")
}

InitNDAPDb =function() 
{
  InitMongoDb("221.133.243.54:3401","NDAPReader","Reader@Galaxy")
}

CreateMongoConnection = function(arg_strAddress,arg_strUser,arg_strPassword,arg_strDb)
{
  connection <- mongo.create(host=arg_strAddress,username=arg_strUser,password=arg_strPassword,db=arg_strDb);
  connection
}
GetTicksFromMongo = function(arg_dbConnection,arg_strContractName,arg_dtimeStart,arg_dtimeNext,arg_bRemoveId=T)
{
  #DbName:ticks
  #CollectionName: IF1401.Tick
  collection = paste("ticks",arg_strContractName,"Tick",sep=".")
  query = mongo.bson.empty()#mongo.bson.from.list(list(name="John"))
  sort = mongo.bson.empty()
  fields = mongo.bson.empty()
  limit = 1L
  skip = 0L
  options = 0L
  it = mongo.find(arg_dbConnection, collection , query ,limit = 10)
  
  
  lst_frm = list()
  
  i=1
  while(mongo.cursor.next(it))  
  {       
    bsonTmp = mongo.cursor.value(it)
    lst = mongo.bson.to.list(bsonTmp)  
    if(arg_bRemoveId)
      lst[["_id"]]<-NULL   
    
    lst[sapply(lst,is.null)]<-NaN
    
    
    lst_frm[[i]] = data.frame(lst)  
    i=i+1
  }
  dfrm = do.call(rbind,lst_frm)
  
  dfrm
}

#返回TFVarieties
GetTFVarietiesFromMongo = function () 
{  
  rows = GetRowsFromMongo_Tool(ndapdb,"NDAP.TFVariety",F)  
  
  cntRow = length(rows)  
  cols = Rows2Columns_Tool(rows)
  
  cols = list( cols$'_id', GetDate(cols$LastTradeDate), GetYM(GetDate(cols$LastTradeDate)))
  names(cols)= c("TFname" ,"LastTradeDate","settlementMonth")
    
  cols    
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
    if( rows[[i]][[13]]!=1 ||  rows[[i]]$frequency!=2)
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

#返回TFBONDEX中的国债信息
#其中国债是历届TF的可交割债券
GetUsefulTreasureInfosFromMongo = function () 
{  
  rows = GetRowsFromMongo_Tool(ndapdb,"NDAP.TFBONDEX",F)  
  
  cols = Rows2Columns_Tool(rows)
  
  cols = list( cols$'_id',cols$Name,cols$issuedate,cols$maturitydate,cols$couponrate,cols$frequency)
  names(cols)= c("code.IB","name" ,"issuedate","maturitydate","couponrate","frequency")
  
  
  cols$issuedate= format( GetDate(cols$issuedate) ,format="%Y/%m/%d")
  cols$maturitydate= format(GetDate(cols$maturitydate),format="%Y/%m/%d")
  
  cols    
}

#返回某一只期货的可交割国债信息
GetDeliveryTreasureInfosFromMongo = function (arg_strTFName) 
{  
  strCollect = paste("NDAP",arg_strTFName , "Bond", sep=".")
  rows = GetRowsFromMongo_Tool(ndapdb,strCollect,F)  
  
  cols = Rows2Columns_Tool(rows)
  
  cols = list( cols$'_id',cols$Name,cols$issuedate,cols$maturitydate,cols$couponrate,cols$frequency)
  names(cols)= c("code.IB","name" ,"issuedate","maturitydate","couponrate","frequency")
  
  
  cols$issuedate= format( GetDate(cols$issuedate) ,format="%Y/%m/%d")
  cols$maturitydate= format(GetDate(cols$maturitydate),format="%Y/%m/%d")
  
  cols    
}

#返回R1MR3M
#[Obsolete("You should not use this fn to get repo datas as this fn get data from an obsolete db.")]
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

#R001 #R007 #R014 #R021 #R1M #R2M #R3M #R4M #R6M #R9M
#open close average
GetQuoteMoneyMarketFromMongoDb = function(arg_reponame , arg_dataname )
{
  strCollection = paste("REPO_",arg_reponame,".IB",sep="")
  strNS = paste("NDAP",strCollection , sep=".");
  cols = GetColumnsFromMongo_Tool(ndapdb,strNS,F)
  #date,open,high,low,close,average,volume,holding
  #R1MR3M = list(  format(as.POSIXct(cols$'_id', origin="1970-01-01 08:00:00"),format="%Y/%m/%d") ,cols$R1M,cols$R3M)
  repo = list(  GetDate(cols$'_id') ,cols[[arg_dataname]])
  names(repo)=c("date","R1M")
  repo
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
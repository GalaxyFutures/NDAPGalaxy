library(rmongodb)


####### Get Datas from MongoDb #######
##Summary:
##Range: Internal
##Args:
#arg_db: Instance of MongoDb
#arg_strNS: Namespace  ("NDAP.BondVariety")
#arg_bRemoveId: Remove the "_id" column from datas
##Return:
#Return: Date Frame
GetDatasMongo_Tool = function(arg_db , arg_strNS , arg_bRemoveId)
{  
  if(!mongo.is.connected(arg_db))
    print("Mongo is not be connected")
  
  lst_frm = list()
  
  it = mongo.find(arg_db,arg_strNS,mongo.bson.empty())  
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




####### Get Datas from MongoDb #######
##Summary:
##Range: Internal
##Args:
#arg_db: Instance of MongoDb
#arg_strNS: Namespace  ("NDAP.BondVariety")
#arg_bRemoveId: Remove the "_id" column from datas
##Return:
#Return: Lists ( lsts$Column1 lsts$Column2 )
GetColumnsFromMongo_Tool = function(arg_db , arg_strNS , arg_bRemoveId)
{  
  if(!mongo.is.connected(arg_db))
    print("Mongo is not be connected")
  
  rows = list() 
  it = mongo.find(arg_db,arg_strNS,mongo.bson.empty())  
  i=1
  while(mongo.cursor.next(it))  
  {       
    bsonTmp = mongo.cursor.value(it)
    lst = mongo.bson.to.list(bsonTmp)  
    if(arg_bRemoveId)
      lst[["_id"]]<-NULL   
    
    lst[sapply(lst,is.null)]<-NaN
    rows[[i]]<-lst        
    i=i+1
  }
  
  rowCount = length(rows)
  colCount=length(rows[[1]])
  
  cols = list()
  for(ic in 1:colCount)
  {
    ########NOTICE########
    ##已知在处理MongoDb.BondVariety时
    ##设置Market类型为vector时，以下语句无法通过：
    ##cols[[ic]][[ir]]=rows[[ir]][[ic]]
    ##Market被MongoDb识别为characters
    ##但是无法被vector添加处理
    ##屏蔽以下语句    
    ##屏蔽语句开始
    #cols[[ic]]=rep(rows[[1]][[ic]], rowCount)
    ##屏蔽语句结束
    #设置为List就没有问题
    #同时,最后形成的List仍然将被unlist为vector
    cols[[ic]]=list()
    cols[[ic]][[rowCount]]=rows[[1]][[ic]]#预先配置内存
  }
  for(ic in 1:colCount)
  {
    for(ir in 1:rowCount)
      cols[[ic]][[ir]]=rows[[ir]][[ic]]
    #将List变为vector
    cols[[ic]]=unlist(cols[[ic]])
  }
    
  names(cols)=names(rows[[1]])
      
  cols
}



GetRowsFromMongo_Tool = function(arg_db , arg_strNS , arg_bRemoveId)
{  
  if(!mongo.is.connected(arg_db))
    print("Mongo is not be connected")
  
  rows = list() 
  it = mongo.find(arg_db,arg_strNS,mongo.bson.empty())  
  i=1
  while(mongo.cursor.next(it))  
  {       
    bsonTmp = mongo.cursor.value(it)
    lst = mongo.bson.to.list(bsonTmp)  
    if(arg_bRemoveId)
      lst[["_id"]]<-NULL   
    
    lst[sapply(lst,is.null)]<-NaN
    rows[[i]]<-lst        
    i=i+1
  }
  rows
}

Rows2Columns_Tool = function( arg_rows )
{
  rows = arg_rows
  rowCount = length(rows)
  colCount=length(rows[[1]])
  
  cols = list()
  for(ic in 1:colCount)
  {
    ########NOTICE########
    ##已知在处理MongoDb.BondVariety时
    ##设置Market类型为vector时，以下语句无法通过：
    ##cols[[ic]][[ir]]=rows[[ir]][[ic]]
    ##Market被MongoDb识别为characters
    ##但是无法被vector添加处理
    ##屏蔽以下语句    
    ##屏蔽语句开始
    #cols[[ic]]=rep(rows[[1]][[ic]], rowCount)
    ##屏蔽语句结束
    #设置为List就没有问题
    #同时,最后形成的List仍然将被unlist为vector
    cols[[ic]]=list()
    cols[[ic]][[rowCount]]=rows[[1]][[ic]]#预先配置内存
  }
  for(ic in 1:colCount)
  {
    for(ir in 1:rowCount)
      cols[[ic]][[ir]]=rows[[ir]][[ic]]
    #将List变为vector
    cols[[ic]]=unlist(cols[[ic]])
  }
  
  names(cols)=names(rows[[1]])
  
  cols
}
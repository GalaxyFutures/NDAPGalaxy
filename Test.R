source("CSharpR.R")
source("MongoHelper.R")
source("MongoDb2Data.R")

connection = CreateMongoConnection("101.95.130.222:3331","zhangguobing","zhangguobing","ticks")


#详细的列表名字请参看MongoDb中的数据结构
datas = GetTicksFromMongo(connection,"TF1403", "2014-01-10 11:15:00","2014-01-10 13:15:00",0,list(UpdateTime=1,lastPrice=1,VarietyName=1))

datas

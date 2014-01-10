source("MongoHelper.R")
source("MongoDb2Data.R")

connection = CreateMongoConnection("101.95.130.222:3331","zhangguobing","zhangguobing","ticks")


datas = GetTicksFromMongo(connection,"TF1403",1)

datas
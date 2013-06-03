#######################################################################
################### Loading Data  #####################################


InitMongoDb( "221.133.243.54")

TFNames = c("TF1203","TF1206","TF1209","TF1212","TF1303","TF1306","TF1309","TF1312")
########## TF Basic Info #########################
TFInfo = list(
  TFname          = TFNames,
  settlementDate  = c(as.Date("2012-03-09","%Y-%m-%d"),
                      as.Date("2012-06-13","%Y-%m-%d"),
                      as.Date("2012-09-19","%Y-%m-%d"),
                      as.Date("2012-12-19","%Y-%m-%d"),
                      as.Date("2013-03-13","%Y-%m-%d"),
                      as.Date("2013-06-19","%Y-%m-%d"),
                      as.Date("2013-09-18","%Y-%m-%d"),
                      as.Date("2013-12-18","%Y-%m-%d")),
  settlementMonth = c(
    as.Date("2012-03-01","%Y-%m-%d"),
    as.Date("2012-06-01","%Y-%m-%d"),
    as.Date("2012-09-01","%Y-%m-%d"),
    as.Date("2012-12-01","%Y-%m-%d"),
    as.Date("2013-03-01","%Y-%m-%d"),
    as.Date("2013-06-01","%Y-%m-%d"),
    as.Date("2013-09-01","%Y-%m-%d"),
    as.Date("2013-12-01","%Y-%m-%d"))
)


########## Bond Basic info #########################
##BondInfo结构
##code: \d+\.\W+
##name: 
##issuedate:
##maturitydate:
##couponrate:
##frequency:
##maturityLeft:
#BondInfo <- read.csv(file="D:\\NationalDebt\\Data\\NationalDebtBaseInfo.csv",head=TRUE,sep=",")
#BondInfo = subset(BondInfo,BondInfo$code.IB != "")
#BondInfo = InitGovBondInfo(BondInfo)
DbBondInfo = GetBondInfosFromMongo2()
BondInfo = InitGovBondInfo(DbBondInfo,F)


########## è´§å¸å¸åºå©çèµ°å¿ä¿¡æ¯ #########################
QuoteMoneyMarket <- read.csv(file="D:\\NationalDebt\\Data\\R1MR3M.csv",head=TRUE,sep=",")
QuoteMoneyMarket$date = as.Date(QuoteMoneyMarket$date,"%Y/%m/%d")


########## TF Quote (Daily) #########################
#strQuoteFileDirectory = "D:\\NationalDebt\\Data\\GZQH\\"
#QuoteTF = list(
#  TF1206 = read.csv(file=paste(strQuoteFileDirectory,"TF1206(S_LOST23).csv",sep=""),head=TRUE,sep=","),
#  TF1209 = read.csv(file=paste(strQuoteFileDirectory,"TF1209(SA).csv",sep=""),head=TRUE,sep=","),
#  TF1212 = read.csv(file=paste(strQuoteFileDirectory,"TF1212(SA).csv",sep=""),head=TRUE,sep=","),
#  TF1303 = read.csv(file=paste(strQuoteFileDirectory,"TF1303(S_T0117).csv",sep=""),head=TRUE,sep=","),
#  TF1306 = read.csv(file=paste(strQuoteFileDirectory,"TF1306(S_T0117).csv",sep=""),head=TRUE,sep=",")
#)
#QuoteTF$TF1206$date = as.Date(as.character(QuoteTF$TF1206$date),"%Y/%m/%d")
#QuoteTF$TF1209$date = as.Date(as.character(QuoteTF$TF1209$date),"%Y/%m/%d")
#QuoteTF$TF1212$date = as.Date(as.character(QuoteTF$TF1212$date),"%Y/%m/%d")
#QuoteTF$TF1303$date = as.Date(as.character(QuoteTF$TF1303$date),"%Y/%m/%d")
#QuoteTF$TF1306$date = as.Date(as.character(QuoteTF$TF1306$date),"%Y/%m/%d")
#str(QuoteTF)
QuoteTF = list()
for(i in 1:length(TFNames))
  QuoteTF[[TFNames[i]]] = GetTFDailyDatasFromMongo(TFNames[[i]])



########## Bond Quote (Daily) ########################
#现券中证估值
BondNames = DbBondInfo[["code.IB"]]
QuoteBond = list()
for(i in 1:length(BondNames))
{
  bondname = BondNames[[i]]  
  QuoteBond[[bondname]]= GetBondDailyDatasFromMongo(bondname)
}

strQuoteFileDirectory = "d:\\NationalDebt\\DATA\\XQZZ\\"
QuoteBond = list(
  Bond090032 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(090032).csv",sep=""),head=TRUE,sep=","),
  Bond120003 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(120003).csv",sep=""),head=TRUE,sep=","),
  Bond100005 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100005).csv",sep=""),head=TRUE,sep=","),
  Bond070003 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(070003).csv",sep=""),head=TRUE,sep=","),
  Bond100010 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100010).csv",sep=""),head=TRUE,sep=","),
  Bond100015 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100015).csv",sep=""),head=TRUE,sep=","),
  Bond070010 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(070010).csv",sep=""),head=TRUE,sep=","),
  Bond100022 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100022).csv",sep=""),head=TRUE,sep=","),
  Bond120014 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(120014).csv",sep=""),head=TRUE,sep=","),
  Bond100027 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100027).csv",sep=""),head=TRUE,sep=","),
  Bond100032 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100032).csv",sep=""),head=TRUE,sep=","),
  Bond100038 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100038).csv",sep=""),head=TRUE,sep=","),
  Bond110003 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(110003).csv",sep=""),head=TRUE,sep=","),
  Bond110006 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(110006).csv",sep=""),head=TRUE,sep=","),
  Bond080003 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(080003).csv",sep=""),head=TRUE,sep=","),
  Bond080010 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(080010).csv",sep=""),head=TRUE,sep=","),
  Bond110017 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(110017).csv",sep=""),head=TRUE,sep=","),
  Bond080018 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(080018).csv",sep=""),head=TRUE,sep=","),
  Bond110021 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(110021).csv",sep=""),head=TRUE,sep=","),
  Bond030009 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(030009).csv",sep=""),head=TRUE,sep=","),
  Bond080025 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(080025).csv",sep=""),head=TRUE,sep=","),
  Bond120005 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(120005).csv",sep=""),head=TRUE,sep=","),
  Bond090003 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(090003).csv",sep=""),head=TRUE,sep=","),
  Bond090007 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(090007).csv",sep=""),head=TRUE,sep=","),
  Bond120010 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(120010).csv",sep=""),head=TRUE,sep=","),
  Bond090012 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(090012).csv",sep=""),head=TRUE,sep=","),
  Bond090016 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(090016).csv",sep=""),head=TRUE,sep=","),
  Bond120016 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(120016).csv",sep=""),head=TRUE,sep=","),
  Bond090023 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(090023).csv",sep=""),head=TRUE,sep=","),
  Bond090027 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(090027).csv",sep=""),head=TRUE,sep=","),
  Bond100002 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100002).csv",sep=""),head=TRUE,sep=","),
  Bond100007 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100007).csv",sep=""),head=TRUE,sep=","),
  Bond100012 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100012).csv",sep=""),head=TRUE,sep=","),
  Bond100019 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100019).csv",sep=""),head=TRUE,sep=","),
  Bond100024 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100024).csv",sep=""),head=TRUE,sep=","),
  Bond100031 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100031).csv",sep=""),head=TRUE,sep=","),
  Bond100034 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(100034).csv",sep=""),head=TRUE,sep=","),
  Bond050012 = read.csv(file=paste(strQuoteFileDirectory,"ZZGZ(050012).csv",sep=""),head=TRUE,sep=",")
)
QuoteBond$Bond090032$date = as.Date(as.character(QuoteBond$Bond090032$date),"%Y/%m/%d")
QuoteBond$Bond120003$date = as.Date(as.character(QuoteBond$Bond120003$date),"%Y/%m/%d")
QuoteBond$Bond100005$date = as.Date(as.character(QuoteBond$Bond100005$date),"%Y/%m/%d")
QuoteBond$Bond070003$date = as.Date(as.character(QuoteBond$Bond070003$date),"%Y/%m/%d")
QuoteBond$Bond100010$date = as.Date(as.character(QuoteBond$Bond100010$date),"%Y/%m/%d")
QuoteBond$Bond100015$date = as.Date(as.character(QuoteBond$Bond100015$date),"%Y/%m/%d")
QuoteBond$Bond070010$date = as.Date(as.character(QuoteBond$Bond070010$date),"%Y/%m/%d")
QuoteBond$Bond100022$date = as.Date(as.character(QuoteBond$Bond100022$date),"%Y/%m/%d")
QuoteBond$Bond120014$date = as.Date(as.character(QuoteBond$Bond120014$date),"%Y/%m/%d")
QuoteBond$Bond100027$date = as.Date(as.character(QuoteBond$Bond100027$date),"%Y/%m/%d")
QuoteBond$Bond100032$date = as.Date(as.character(QuoteBond$Bond100032$date),"%Y/%m/%d")
QuoteBond$Bond100038$date = as.Date(as.character(QuoteBond$Bond100038$date),"%Y/%m/%d")
QuoteBond$Bond110003$date = as.Date(as.character(QuoteBond$Bond110003$date),"%Y/%m/%d")
QuoteBond$Bond110006$date = as.Date(as.character(QuoteBond$Bond110006$date),"%Y/%m/%d")
QuoteBond$Bond080003$date = as.Date(as.character(QuoteBond$Bond080003$date),"%Y/%m/%d")
QuoteBond$Bond080010$date = as.Date(as.character(QuoteBond$Bond080010$date),"%Y/%m/%d")
QuoteBond$Bond110017$date = as.Date(as.character(QuoteBond$Bond110017$date),"%Y/%m/%d")
QuoteBond$Bond080018$date = as.Date(as.character(QuoteBond$Bond080018$date),"%Y/%m/%d")
QuoteBond$Bond110021$date = as.Date(as.character(QuoteBond$Bond110021$date),"%Y/%m/%d")
QuoteBond$Bond030009$date = as.Date(as.character(QuoteBond$Bond030009$date),"%Y/%m/%d")
QuoteBond$Bond080025$date = as.Date(as.character(QuoteBond$Bond080025$date),"%Y/%m/%d")
QuoteBond$Bond120005$date = as.Date(as.character(QuoteBond$Bond120005$date),"%Y/%m/%d")
QuoteBond$Bond090003$date = as.Date(as.character(QuoteBond$Bond090003$date),"%Y/%m/%d")
QuoteBond$Bond090007$date = as.Date(as.character(QuoteBond$Bond090007$date),"%Y/%m/%d")
QuoteBond$Bond120010$date = as.Date(as.character(QuoteBond$Bond120010$date),"%Y/%m/%d")
QuoteBond$Bond090012$date = as.Date(as.character(QuoteBond$Bond090012$date),"%Y/%m/%d")
QuoteBond$Bond090016$date = as.Date(as.character(QuoteBond$Bond090016$date),"%Y/%m/%d")
QuoteBond$Bond120016$date = as.Date(as.character(QuoteBond$Bond120016$date),"%Y/%m/%d")
QuoteBond$Bond090023$date = as.Date(as.character(QuoteBond$Bond090023$date),"%Y/%m/%d")
QuoteBond$Bond090027$date = as.Date(as.character(QuoteBond$Bond090027$date),"%Y/%m/%d")
QuoteBond$Bond100002$date = as.Date(as.character(QuoteBond$Bond100002$date),"%Y/%m/%d")
QuoteBond$Bond100007$date = as.Date(as.character(QuoteBond$Bond100007$date),"%Y/%m/%d")
QuoteBond$Bond100012$date = as.Date(as.character(QuoteBond$Bond100012$date),"%Y/%m/%d")
QuoteBond$Bond100019$date = as.Date(as.character(QuoteBond$Bond100019$date),"%Y/%m/%d")
QuoteBond$Bond100024$date = as.Date(as.character(QuoteBond$Bond100024$date),"%Y/%m/%d")
QuoteBond$Bond100031$date = as.Date(as.character(QuoteBond$Bond100031$date),"%Y/%m/%d")
QuoteBond$Bond100034$date = as.Date(as.character(QuoteBond$Bond100034$date),"%Y/%m/%d")
QuoteBond$Bond050012$date = as.Date(as.character(QuoteBond$Bond050012$date),"%Y/%m/%d")
str(QuoteBond)
########## 5Y YTM and R1M å¯¹æ¯æ°æ® ########################
QuoteYTMandR1M = read.csv(file="D:\\NationalDebt\\Data\\R1M.csv",head=TRUE,sep=",")
QuoteYTMandR1M$date = as.Date(QuoteYTMandR1M$date,"%Y/%m/%d")
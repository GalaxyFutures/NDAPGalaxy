#######################################################################
################### Loading Data  #####################################

<<<<<<< HEAD
###Date:2013/06/04
###Change From WangHua
=======
###Date:2013/06/05
###Hello world!!bbbbb
###It's me!
>>>>>>> 40d01998a10c0f9e77b4e7a0fc9df105beaafb7a

InitMongoDb("221.133.243.54","NDAPReader","Reader@Galaxy")

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
BondInfo = InitGovBondInfo( DbBondInfo )



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


########## 5Y YTM and R1M å¯¹æ¯æ°æ® ########################
QuoteYTMandR1M = read.csv(file="D:\\NationalDebt\\Data\\R1M.csv",head=TRUE,sep=",")
QuoteYTMandR1M$date = as.Date(QuoteYTMandR1M$date,"%Y/%m/%d")


#输入一：日期
today = as.Date("2013/07/15","%Y/%m/%d")

#输入二：YtmShift
YtmShift = c(-100,-50,0,50,100)

#输入三：选定某个国债期货
TFName = "TF1403"

BondInfo_temp = InitGovBondInfo(DbBondInfo)
BondInfo_temp = ResetToday(BondInfo_temp,"GOV",today,FALSE,FALSE,TRUE)
BondInfo_temp = InitBondPrice(BondInfo_temp,"GOV",QuoteBond)
BasisDiff = CTDScenario(BondInfo_temp,"GOV",QuoteBond,TFName,YtmShift)

BasisDiff
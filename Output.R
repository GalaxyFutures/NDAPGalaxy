
#######################################################################
################### Start Research ####################################
##说明短期资金利率和长端利率走势的不相关性质
plot(diff(QuoteYTMandR1M$YTMgov5year),diff(QuoteYTMandR1M$R1M),xlim = c(-0.3,0.3),ylim = c(-0.5,0.5),
     main = "5年期国债收益率和银行间1月回购关系图",
     xlab = "5年期国债收益率日变化值",
     ylab = "银行间1月回购日变化值");
plot(diff(QuoteYTMandR1M$YTMgov5year),diff(QuoteYTMandR1M$YTMgov3year),xlim = c(-0.3,0.3),ylim = c(-0.5,0.5))
lm.YTM5YR1M = lm(diff(QuoteYTMandR1M$YTMgov5year)~diff(QuoteYTMandR1M$R1M))
anova(lm.YTM5YR1M)
summary(lm.YTM5YR1M)
#######################################################################
#################### test code     ####################################
str(BondInfo)


#to 沈迪 请执行这部分代码
BondInfo = ResetToday(BondInfo,"GOV","2012-11-01")
#BondInfo = ResetToday(BondInfo,"GOV","2013-12-18")
BondInfo = AddTFInfo(BondInfo,"GOV",TFInfo)
BondInfo = InitBondPrice(BondInfo,"GOV",QuoteBond)
BondInfo = InitTFPrice(BondInfo,"GOV",QuoteTF)

BondInfo = CalculateExpectedTFPrice(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)
BondInfo = CalculateNetBasis(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)
BondInfo = CalculateIRR(BondInfo,"GOV",TFInfo,QuoteBond,QuoteTF,QuoteMoneyMarket)

BondInfo = InitBPV(BondInfo,"GOV", QuoteBond) 
BondInfo = FindCTD(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)
BondInfo = CalculateBPVTF(BondInfo,"GOV",TFInfo,QuoteBond,QuoteMoneyMarket)

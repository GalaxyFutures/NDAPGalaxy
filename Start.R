
ChineseOS = False

if(ChineseOS)
{
  #source("CSharpR.R",encoding="utf-8")
  #source("MongoHelper.R",encoding="utf-8")
  #source("MongoDb2Data.R",encoding="utf-8")
  source("HELPER.R",encoding="utf-8")
  #source("MyData.R",encoding="utf-8")
  source("CTDScenario.R",encoding="utf-8")
  source("CalculateRepoRelated.R",encoding="utf-8")
}
else
{
  #source("CSharpR.R")
  #source("MongoHelper.R")
  #source("MongoDb2Data.R")
  source("HELPER.R")
  source("CTDScenario.R")
  #source("MyData.R")
  source("CalculateRepoRelated.R")
}

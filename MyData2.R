
InitMongoDb("221.133.243.54:3401","NDAPReader","Reader@Galaxy")

TFInfo = GetTFVarietiesFromMongo()


DbBondInfo = GetUsefulTreasureInfosFromMongo()#GetTreasureInfosFromMongo() 
BondInfo = InitGovBondInfo( DbBondInfo )

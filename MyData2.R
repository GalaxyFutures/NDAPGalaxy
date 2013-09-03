
InitMongoDb("221.133.243.54:3331","NDAPReader","Reader@Galaxy")

TFInfo = GetTFVarietiesFromMongo()


DbBondInfo = GetUsefulTreasureInfosFromMongo()#GetTreasureInfosFromMongo() 
BondInfo = InitGovBondInfo( DbBondInfo )

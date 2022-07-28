FUNCTION Start() AS VOID

DbUseArea(TRUE,"DBFVFP","C:\xSharp\_Robert\data\orders.dbf")

? DbOrderInfo( DBOI_EXPRESSION )	// empty in VFP, "customerid" in VO dialect
? DbOrderInfo( DBOI_CONDITION ) 	// empty in both
? DbOrderInfo( DBOI_ISDESC ) 		// false in both
? DbOrderInfo( DBOI_RECNO )  		// 1 in VFP, 0 in VO dialect
? DbOrderInfo( DBOI_POSITION ) 		// 1 in VFP, 0 in VO dialect

DbCloseArea()

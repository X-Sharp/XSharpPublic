FUNCTION Start() AS VOID

    ? DbUseArea(TRUE,"DBFVFP","C:\Test\customers.dbf")
    ? DbUseArea(TRUE,"DBFVFP","C:\Test2\employee.dbf")
? DbUseArea(TRUE,"DBFVFP","C:\Test\orders.dbf")

? DbOrderInfo( DBOI_EXPRESSION )	// empty in VFP, "customerid" in VO dialect
? DbOrderInfo( DBOI_CONDITION ) 	// empty in both
? DbOrderInfo( DBOI_ISDESC ) 		// false in both
? DbOrderInfo( DBOI_RECNO )  		// 1 in VFP, 0 in VO dialect
? DbOrderInfo( DBOI_POSITION ) 		// 1 in VFP, 0 in VO dialect

LOCAL ARRAY aDb[1]
? ADatabases(aDb)
ShowArray(aDb)
DbCloseArea()
WAIT

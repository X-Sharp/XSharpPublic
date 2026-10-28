
FUNCTION Start( ) AS VOID
	FIELD Lastname
	RddSetDefault("DBFVFP")
	CREATE CURSOR Customer (Lastname C(10) )
	INSERT INTO Customer Values ( "Foray" )
	INSERT INTO Customer Values ("Hulst"  )
	INSERT INTO Customer Values ("Kokkalis" )
	INSERT INTO Customer Values ("Pyrgas"    )
	INSERT INTO Customer Values ("Rodriguez"  )
	// The following was not recognized in 2.10
	SCAN FOR LastName = "Hulst" .and. ! Deleted()
		? LastName
	END SCAN
	USE
RETURN

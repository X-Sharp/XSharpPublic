
FUNCTION Start( ) AS VOID
	FIELD Lastname
	USE "Customer"
	// The following was not recognized in 2.10
	SCAN FOR LastName == "Hulst" .and. ! Deleted()
		? LastName
	END SCAN
	USE	
RETURN

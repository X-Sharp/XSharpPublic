// 676. error XS8050: Only auto-implemented properties can have initializers.
FUNCTION Start() AS VOID

RETURN

CLASS TestClass
	EXPORT n AS INT

	// error XS8050: Only auto-implemented properties can have initializers.
	PROPERTY TestProp1 AS STRING   
		SET       
		END SET
	END PROPERTY

	// all following compile without errors
	PROPERTY TestProp2 AS STRING
		GET
			RETURN ""
		END GET
	END PROPERTY

	PROPERTY TestProp3 AS STRING
		SET
		END SET
		GET
			RETURN ""
		END GET
	END PROPERTY
	
	PROPERTY TestProp4 AS STRING SET
    PROPERTY TestProp5 AS STRING AUTO

END CLASS

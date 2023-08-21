// 676. error XS8050: Only auto-implemented properties can have initializers.
#pragma warnings (9047, off) // get or set accessor wo body
#pragma warnings (9051, off) // set accessor
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

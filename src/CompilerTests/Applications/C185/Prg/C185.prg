// 185. error XS0111: Type 'SomeClass' already defines a member called 'this' with the same parameter types
FUNCTION Start() AS VOID
LOCAL o AS SomeClass
o := SomeClass{}
? o:IndProp1[1]
? o:IndProp2[1]

CLASS SomeClass
	PROPERTY IndProp1[n AS INT] AS INT
		GET
			RETURN 0
		END
	END PROPERTY
	PROPERTY IndProp2[n AS INT] AS INT
		GET
			RETURN 0
		END
	END PROPERTY
END CLASS


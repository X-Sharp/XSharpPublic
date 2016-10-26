// 184. error XS1061: 'SomeClass' does not contain a definition for 'IndProp' and no extension method 'IndProp' accepting a first argument of type 'SomeClass' could be found (are you missing a using directive or an assembly reference?)
FUNCTION Start() AS VOID
LOCAL o AS SomeClass
o := SomeClass{}
? o:IndProp[1]

CLASS SomeClass
	PROPERTY IndProp[n AS INT] AS INT
		GET
			RETURN 0
		END
	END PROPERTY
END CLASS


// 196. error XS0102: The type 'TestClass' already contains a definition for 'TestProp'
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
? o:TestProp[2]
? o:TestProp["a"]

CLASS TestClass
	PROPERTY TestProp[n AS INT] AS INT
	GET
		RETURN n * 2
	END GET
	END PROPERTY
	PROPERTY TestProp[c AS STRING] AS STRING
	GET
		RETURN c + "a"
	END GET
	END PROPERTY
END CLASS


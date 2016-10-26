// 46. error XS0549: 'TestClass.TestProp.get' is a new virtual member in sealed class 'TestClass'
SEALED CLASS TestClass
VIRTUAL PROPERTY TestProp AS INT
	GET
		RETURN 0
	END GET
END PROPERTY
END CLASS


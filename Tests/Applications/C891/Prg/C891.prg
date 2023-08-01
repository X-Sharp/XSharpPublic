// 891. Internal compiler error with incorrect explicit interface implementation with property
// https://github.com/X-Sharp/XSharpPublic/issues/1306
CLASS TestClass

// internal compiler error:
PROPERTY Foo.Bar[n AS INT] AS INT
	GET
		RETURN 123
	END GET
END PROPERTY

// this one is ok (reports error as expected):
METHOD ITest.Bar(n AS INT) AS INT
RETURN 0

END CLASS


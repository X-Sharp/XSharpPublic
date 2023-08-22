// 620. error XS1501: No overload for method 'StaticInstanceMethod' takes 3 arguments
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	o:StaticInstanceMethod(1,FALSE, "")
	TestClass.StaticInstanceMethod(2)
RETURN

CLASS TestClass
	METHOD Test() AS VOID
		StaticInstanceMethod(NULL , FALSE , "")
	RETURN
	
	METHOD StaticInstanceMethod(o AS OBJECT , l AS LOGIC , cPrevName AS STRING) AS VOID

	STATIC METHOD StaticInstanceMethod(o AS OBJECT) AS VOID
END CLASS


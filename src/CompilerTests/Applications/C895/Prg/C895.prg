// 895. initlocals compiler option also initializes class fields
// https://www.xsharp.eu/forum/topic?t=3543&p=28258#p28258
// https://github.com/X-Sharp/XSharpPublic/issues/1408
FUNCTION Start() AS VOID
	LOCAL o := Child{} AS Child
	? o:nTest1, o:nTest2
	? o:cTest1, o:cTest2
	
	xAssert(o:nTest1 == 1)
	xAssert(o:nTest2 == 2)
	xAssert(o:cTest1 == "1")
	xAssert(o:cTest2 == "2")
	
	FOR LOCAL n := 1 AS INT UPTO 10
		LOCAL mInit AS INT
		xAssert(mInit == 0)
		mInit ++
		xAssert(mInit == 1)
		? mInit
	NEXT
	
	
RETURN

CLASS Parent
	EXPORT nTest1 AS INT
	EXPORT nTest2 AS INT

	EXPORT cTest1 AS STRING
	EXPORT cTest2 AS STRING
END CLASS

CLASS Child INHERIT Parent
	CONSTRUCTOR()
		SELF:nTest1 := 1
		SELF:cTest1 := "1"
		SUPER()
		SELF:nTest2 := 2
		SELF:cTest2 := "2"
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

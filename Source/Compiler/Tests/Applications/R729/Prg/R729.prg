// /vo7+

FUNCTION Start() AS VOID
	LOCAL o,u AS USUAL
	LOCAL t AS TestClass
	local s as string
	o := TestClass{}
	u := "OLD"

	//? "Late bound Clipper method with @:"
	o:ClipMeth(@u,1)
	xAssert(u == "NEW")
	// OLD, <ptr value>, OLD
	// "Late bound Clipper method with REF"
	u := "OLD"
	o:ClipMeth( REF u)
	// OLD, <ptr value>, OLD
	xAssert(u == "NEW")
	t := TestClass{}
	u := "OLD"
	// "Early bound Clipper method with @:"
	t:ClipMeth(@u,"abc")
	// OLD, OLD, OLD                                                         1
	xAssert(u == "NEW")
	// "Early bound Clipper method with REF:"
	u := "OLD"
	o:ClipMeth(REF u)
	// OLD, OLD, NEW   
	xAssert(u == "NEW")
	s := "Robert"
	TestByRef(@s)
	xAssert(s == "Chris")
	s := "Robert"
	TestByRef(REF s)   
	xAssert(s == "Chris")
	LOCAL x IS testStruct
	x:Value := 21
	TestPtr(@x)
	xAssert(x:Value == 42)
RETURN

CLASS TestClass
	METHOD ClipMeth(u)
		xAssert(u == "OLD")
		u := "NEW"
	RETURN NIL
END CLASS



FUNCTION TestByRef(s REF STRING) as void
	xAssert(s == "Robert")
	s := "Chris"
	return
FUNCTION TestPtr( p as testStruct) as void
	xAssert(p:Value == 21)
	p:Value *= 2
	RETURN
	

VOSTRUCT testStruct
	MEMBER Value as LONG
	
	
PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN		

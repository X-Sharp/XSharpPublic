// https://github.com/X-Sharp/XSharpPublic/issues/1453
// We see this in several VFP apps
FUNCTION Start( ) AS VOID
	local cVar  as string
	cVar := "abc ;    // line continuation
	def"
	cVar := cVar:Replace(" ","")
	cVar := cVar:Replace(e"\t","")
	? cVar
	xAssert(cVar == "abcdef")
	cVar := "123 ;    // line continuation
	456 ;              && more
	789"
	cVar := cVar:Replace(" ","")
	cVar := cVar:Replace(e"\t","")
	? cVar
	xAssert(cVar == "123456789")
RETURN



PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

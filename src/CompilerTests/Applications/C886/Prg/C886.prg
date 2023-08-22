// 886. VO incompatibility with assigning NIL to SYMBOL
// https://github.com/X-Sharp/XSharpPublic/issues/1231

FUNCTION Start() AS VOID
	LOCAL s AS SYMBOL
	s := NIL
	xAssert( .not. IsNil(s) )
	xAssert( s == NULL_SYMBOL )
	
	xAssert( .not. IsSymbolNil(NIL) )
	xAssert( .not. IsSymbolNil() )
	xAssert( .not. IsSymbolNil(#ABC) )
	
	
FUNCTION IsSymbolNil(sym := NIL AS SYMBOL) AS LOGIC
RETURN IsNil(sym)

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN



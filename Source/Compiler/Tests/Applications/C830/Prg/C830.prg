// 830. No runtime exception when casting to the wrong class with /lb+ or /vo7+ enabled
// https://github.com/X-Sharp/XSharpPublic/issues/961
// With either /lb+ or /vo7+ enabled, there's no runtime exception when casting the wrong class to another
// When both options are disabled, then a System.InvalidCastException is thrown as excpected
#pragma warnings(165, off) // unasigned local
#pragma warnings(162, off) // unreachable code
#pragma warnings(9101, off) // try without catch
FUNCTION Start() AS VOID STRICT
	LOCAL x AS OBJECT
    LOCAL y AS BaseClass2
    LOCAL lNoException := FALSE  AS LOGIC

    x := BaseClass1{}

    TRY
	    y := x
	    lNoException := TRUE
    END TRY

    TRY
	    y := (BaseClass2)x
	    lNoException := TRUE
    END TRY

    IF lNoException
	    y:Test2() // BaseClass2

	    ? y:GetType():ToString() // BaseClass1

	    THROW Exception{"Above should had resulted to a System.InvalidCastException"}
    ELSE
    	RETURN
    ENDIF

RETURN

CLASS BaseClass1
    METHOD Test() AS VOID
    ? "BaseClass1"
END CLASS

CLASS BaseClass2
	PROTECT n := 123 AS INT
    METHOD Test2() AS VOID
    ? "BaseClass2"
    ? SELF:n // 0
END CLASS

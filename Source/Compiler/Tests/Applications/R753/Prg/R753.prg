USING System.Globalization
FUNCTION Start( ) AS VOID
	LOCAL c AS STRING
	c := "X# supports Local functions"

    SimpleFunction(c)
    
    WithPsz("Some test", OUT VAR s)      
    xAssert(s == "SO")
    
    xAssert(Generics("123") == 123.0)
    xAssert(Generics(123.45m) == 123.45)
    xAssert(Generics($123.45) == 123.45)
    xAssert(Generics(0x123456) == 1193046.00)
	
	LOCAL FUNCTION Generics<T>(s1 AS T) AS REAL8 WHERE T IS IConvertible
	    ? "Generic parameters "
	    VAR ic := (IConvertible) s1
	    ? s1:GetType():Name, s1
	    ? ic:ToDouble(CultureInfo{"en-US"})    
	    ?        
	    RETURN ic:ToDouble(CultureInfo{"en-US"})    
    END FUNCTION
	
    LOCAL FUNCTION SimpleFunction(d AS STRING) AS STRING
        ? "Simple function"
        ?  c
        xAssert(c == d)
        ?
        RETURN c	
    END FUNCTION

	LOCAL PROCEDURE WithPsz(s AS STRING, s2 OUT STRING)
	    LOCAL p AS PSZ
	    ? "Procedure with PSZ variables"
	    p := String2Psz(Left(Upper(s),2))
        s2 := Psz2String(p)	    
	    ?  s, s2                                      
	    ?
    END PROCEDURE

RETURN


PROC xAssert(l AS LOGIC) 
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN


// error XS1739: The best overload for 'Multiply' does not have a parameter named 'r8Value'
// reported by Johan, cannot use inline assignments to vars inside function/methods calls in Core dialect
// works ok in vulcan dialect

/*
      !!! IMPORTANT !!!

In case it is not possible to allow this in Core dialect, please have a look at the
second part of the report. In the case of class method/constructor, currently no compiler
error is reported also in Core when using the inline syntax, but the inline assignment value 
is not actually assigned to the variable, which will lead to unpredictable untime behavior.
In Vulcan dialect it works as expected also at runtime.

So if it is not possible to make this work correctly in core, then a compiler error should
be reported also in the case of method/constructor calls
*/
FUNCTION Start() AS VOID
    LOCAL r8Value := 0.0 AS REAL8
    LOCAL result := 0.0 AS REAL8

    result := Multiply(r8Value := 3.0, 5)
    ?result , r8Value
    xAssert(r8Value == 3.0)
    xAssert(result == 15.0)
    
    result := Multiply(5.0, r8Value := 10)
    ?result, r8Value
    xAssert(r8Value == 10.0)
    xAssert(result == 50.0)

    LOCAL o AS TestClass
    LOCAL n AS INT
    
    // the following compiles without errors also in core, but the value does not get assigned to the local
    o := TestClass{n := 1}
    ? n
    xAssert(n == 1)
    o:Test(n := 2)
    ? n
    xAssert(n == 2)
RETURN

FUNCTION Multiply(r1 AS REAL8, r2 AS REAL8) AS REAL8
RETURN r1 * r2

CLASS TestClass
	CONSTRUCTOR(n AS INT)
		? n
	RETURN
	METHOD Test(n AS INT) AS VOID
		? n
	RETURN
	
END CLASS


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"


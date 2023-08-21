// R773 Interpolated Expression in the VO Dialect: use :: for Format specifier
FUNCTION Start( ) AS VOID
    System.Globalization.CultureInfo.CurrentUICulture := System.Globalization.CultureInfo.GetCultureInfo("nl-nl")
    System.Globalization.CultureInfo.CurrentCulture := System.Globalization.CultureInfo.GetCultureInfo("nl-nl")
    TestColon()
    TestDot()

RETURN
#pragma options("AllowDot", OFF)
FUNCTION TestColon AS VOID
	LOCAL i AS LONG
	i := 42
	? i"i = {i:ToString()}"     // when AllowDot = OFF then the colon = send operator
	xAssert(i"{i:ToString()}" == "42")
	? i"i = {i::F2}"            // and "::" is used as Format separator
	xAssert(i"{i::F2}" == "42,00")
RETURN
    
#pragma options("AllowDot", ON)    
FUNCTION TestDot AS VOID
	LOCAL i AS LONG
	i := 42
	? i"i = {i.ToString()}"     // when AllowDot = OFF then the DOT = send operator
	xAssert(i"{i.ToString()}" == "42")
	? i"i = {i:F2}"             // and ":" is used as Format separator
	xAssert(i"{i:F2}" == "42,00")
RETURN
    
    
    
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN    

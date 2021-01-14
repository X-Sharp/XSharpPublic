// R759: Paren Expression inside codeblock: the Codeblock parameters must be passed to the local function so it can use them
// The compiler should pass the variables by reference to the local function so the values inside the codeblock are also updated.
FUNCTION Start( ) AS VOID  
LOCAL aRes := {} AS ARRAY
LOCAL aValues := {} AS ARRAY
AADD(aValues, NIL)  
AADD(aValues, 100)                                                      
AEval(aValues, {|x,y,z| AAdd(aRes, iif (IsNil(x), (y := 10, z := 32, x := y + z), x))})
xAssert(ALen(aRes) == 2)
xAssert(aRes[1] == 42)
xAssert(aRes[2] == 100)
RETURN



PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN

/*
The generated local function looks like this:

	__Usual f(ref __Usual x, ref __Usual y, ref __Usual z)
	{
		y = 10;
		z = 32;
		return x = y + z;
	}

And the codeblock is converted to this:

    (<>F<__Usual, __Usual, __Usual, __Usual>)((__Usual x, __Usual y, __Usual z) => 
    XSharp.RT.Functions.AAdd(aRes, XSharp.RT.Functions.IsNil(x) ? f(ref x, ref y, ref z) : x))

*/

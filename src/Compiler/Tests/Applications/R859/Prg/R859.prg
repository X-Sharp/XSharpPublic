
FUNCTION Start( ) AS VOID
local o as Object
local u as USUAL
u := 42
o := __castclass(object, u) // this should generate a box of the usual
? o:GetType():Name
xAssert(o:GetType():Name == "__Usual")
u := __castclass(usual, o)  // this should unbox the object into the usual
? u

o := u  // this extracts the long from the usual;
? o:GetType():Name
xAssert(o:GetType():Name == "Int32")
try
    u := __castclass(usual, o)  // unboxing a long into a usual fails
    // should not get here
    xAssert(false)
catch e as Exception
    // expect exception
    xAssert(e is InvalidCastException)
end try




RETURN


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

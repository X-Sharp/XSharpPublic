// 729. Problems with macro substitution
// https://github.com/X-Sharp/XSharpPublic/issues/470

/*
error XS0131: The left-hand side of an assignment must be a variable, property or indexer

System.Security.VerificationException
Operation could destabilize the runtime.

Callstack : 
Object XSharp.RuntimeCodeblock.EvalBlock(System.Object[] args)
XSharp._Codeblock.__Usual XSharp._Codeblock.Eval(XSharp.__Usual[] args)
Object XSharp.Codeblock.EvalBlock(System.Object[] args)
static __Usual XSharp.RT.Functions.Evaluate(System.String cString, System.Boolean lAllowSingleQuotes)
static __Usual XSharp.RT.Functions.Evaluate(System.String cString)
static System.Void fox.Exe.Functions.Start()
*/

FUNCTION Start() AS VOID
LOCAL obj
obj = createobject("empty")
AddProperty(obj,"MyName","Loy")
? obj.MyName  // prints "Loy" - this works
xAssert(obj.MyName == "Loy")

LOCAL macrovar
macrovar = "obj.MyName"
? &macrovar // Unhandled Exception: System.Security.VerificationException: Operation could destabilize the runtime.
xAssert(&macrovar == "Loy")
	
//This one works when you get the value of the property
LOCAL prop
prop = "MyName"
? obj.&prop // prints "Loy" - OK
xAssert(obj.MyName == "Loy")

//But, this one errors out when setting the value of the property
obj.&prop = "Jack"  // error XS0131: The left-hand side of an assignment must be a variable, property or indexer
? obj.&prop
xAssert(obj.MyName == "Jack")

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

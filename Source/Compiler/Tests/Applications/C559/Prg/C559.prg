// 559. No compiler error with /vo15- and untyped parameters

// /vo15-

FUNCTION Start() AS VOID

RETURN

// correct error on those 2: error XS1031: Type expected
FUNCTION NoReturnType(n AS INT)  
RETURN NIL
FUNCTION NoLocalType() AS VOID
LOCAL a
RETURN 

// error here:
FUNCTION NoParamType(a,b) AS INT 
RETURN 0
// but no error here:
FUNCTION ClipperCallingConvNoParamTypeIsOk(a,b) AS INT  CLIPPER
RETURN 0


// or here:
CLASS TestClass
	// Error here
METHOD NoParamType(a,b) AS INT  
RETURN 0                         
// but not here
METHOD ClipperCallingConvNoParamTypeIsOk(a,b) AS INT  CLIPPER
RETURN 0

END CLASS

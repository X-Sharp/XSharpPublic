// 3. error XS0165: Use of unassigned local variable 'cRet'
// In vulcan this is a warning
#pragma warnings(165, off)    // use of unassigned local variable
FUNCTION Start() AS VOID
	? Test()
FUNCTION Test() AS STRING
	LOCAL cRet AS STRING
RETURN cRet


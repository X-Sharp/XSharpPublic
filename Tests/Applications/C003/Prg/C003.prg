// 3. error XS0165: Use of unassigned local variable 'cRet'
// In vulcan this is a warning
 
FUNCTION Start() AS VOID
	? Test()
FUNCTION Test() AS STRING
	LOCAL cRet AS STRING
RETURN cRet


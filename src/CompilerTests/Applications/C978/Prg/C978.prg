// 978. Problems with the /fox3 option - codeblocks #2054
// https://github.com/X-Sharp/XSharpPublic/issues/2054

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

CLASS TestClass
	EXPORT expfld AS INT
	METHOD Test() AS VOID
	cDbf := "c:\test\testcb"
	DbCreate( cDbf, {{"FLD","N",10,0}} )
	DbUseArea( , , cdbf )
	DbAppend( )
	DbGoTop()

	LOCAL n AS INT
	COUNT TO n FOR this.expfld == 0
	? n
END CLASS

FUNCTION Start() AS VOID
	TestClass{}:Test()


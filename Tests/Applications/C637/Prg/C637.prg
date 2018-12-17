// error XS1510: A ref or out value must be an assignable variable
/*
Compiled with /vo7 enabled, it reports "A ref or out value must be an assignable variable"
(source taken from the SDK)
*/
FUNCTION Start() AS VOID
	LOCAL DIM abDrive[128] AS BYTE
	LOCAL DIM abDir	 [256]   AS BYTE
	LOCAL DIM abName [260]  AS BYTE
	LOCAL DIM abExt  [128]	AS BYTE
	LOCAL uString AS USUAL
	
	uString := "C:\Test\File.ext"

	SplitPath(uString, PSZ(_CAST, @abDrive[1]), PSZ(_CAST,@abDir[1]), PSZ(_CAST,@abName[1]), PSZ(_CAST,@abExt[1]))
	
	? System.Text.Encoding.Default:GetString(abDrive)
	? System.Text.Encoding.Default:GetString(abDir)
	? System.Text.Encoding.Default:GetString(abName)
	? System.Text.Encoding.Default:GetString(abExt)
	
	CheckResult(System.Text.Encoding.Default:GetString(abDrive) , "C:")
	CheckResult(System.Text.Encoding.Default:GetString(abDir) , "\Test\")
	CheckResult(System.Text.Encoding.Default:GetString(abName) , "File")
	CheckResult(System.Text.Encoding.Default:GetString(abExt) , ".ext")
RETURN

PROCEDURE CheckResult(c AS STRING, cCorrect AS STRING)
	LOCAL nAt AS DWORD
	nAt := At(Chr(0), c)
	c := Left(c, nAt - 1)
	IF c == cCorrect
		? "test passed"
	ELSE
		LOCAL cStackTrace AS STRING
		cStackTrace := "Line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()
		THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" + CRLF + CRLF + cStackTrace , cCorrect , c)}
	ENDIF
RETURN

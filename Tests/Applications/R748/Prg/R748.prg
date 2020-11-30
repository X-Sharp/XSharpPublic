
FUNCTION Start( ) AS VOID           
    ? "Hello"
    LOCAL l = GetDesktopWindow()
	xAssert(l!= 0)
	VAR sb := System.Text.StringBuilder{255}
	XAssert(GetSystemDirectory(sb, sb.Capacity+1) != 0) 
	? sb.ToString()
	xAssert(sb.ToString().ToLower().Contains("system32"))
	WAIT
RETURN



DECLARE INTEGER GetActiveWindow     IN user32    
DECLARE INTEGER GetDesktopWindow    IN user32    
DECLARE INTEGER MessageBox          IN USER32 INTEGER  , STRING  , STRING , LONG 
DECLARE INTEGER GetSystemDirectoryA  IN Kernel32 AS GetSystemDirectory System.Text.StringBuilder buffer, LONG numchars
// declaration with STRING @ will generate a compiler error
//DECLARE INTEGER GetSystemDirectory IN Kernel32 STRING @, LONG


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN 

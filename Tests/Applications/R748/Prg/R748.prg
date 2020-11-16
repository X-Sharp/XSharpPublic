
FUNCTION Start( ) AS VOID
	? GetActiveWindow()  
	MessageBox(0, "Text","Caption",0)                      
	VAR sb := System.Text.StringBuilder{255}
	? GetSystemDirectory(sb, sb.Capacity+1)
	? sb.ToString()
	WAIT
RETURN



DECLARE INTEGER GetActiveWindow     IN user32    
DECLARE INTEGER MessageBox          IN USER32 INTEGER  , STRING  , STRING , LONG 
DECLARE INTEGER GetSystemDirectoryA  IN Kernel32 AS GetSystemDirectory System.Text.StringBuilder buffer, LONG numchars
// declaration with STRING @ will generate a compiler error
//DECLARE INTEGER GetSystemDirectory IN Kernel32 STRING @, LONG

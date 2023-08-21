// https://www.xsharp.eu/forum/devteam/2844-test-c820-and-ticket-899
#pragma options("vo7", on)
GLOBAL wordValue AS WORD
FUNCTION Start() AS VOID
   
LOCAL pWBuffer := NULL AS WORD PTR
// error XS1503: Argument 1: cannot convert from 'word**' to 'word*'
// error XS1510: A REF or OUT value must be an assignable variable
wordValue := 42
GetControlClass(@pWBuffer)
? pWBuffer[1]
xAssert(pWBuffer[1] == 42)

LOCAL hKey := NULL AS PTR
// error XS1503: Argument 4: cannot convert from 'PTR*' to 'PTR'
// error XS1510: A REF or OUT value must be an assignable variable
OpenKey(HKEY_CLASSES_ROOT,"test",123,@hKey)
xAssert(hKey != NULL_PTR)
pwBuffer := hKey          
xAssert(pWBuffer[1] == 42)



FUNCTION GetControlClass(pWBuffer REF WORD PTR) AS VOID
    pwBuffer := @wordValue
FUNCTION OpenKey(hKey AS PTR,cSubKey AS STRING,nAccess AS DWORD,hResult REF PTR) AS LOGIC
    hResult := @wordValue
RETURN TRUE



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN		


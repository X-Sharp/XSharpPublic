GLOBAL g AS MySleep PTR
FUNCTION Start( ) AS VOID
   VAR hDll := LoadLibrary("Kernel32.DLL")
   xAssert(hDLL != NULL_PTR)
   g := GetProcAddress(hDLL, "Sleep")
   xAssert(g != NULL_PTR)
   test()	
RETURN

STATIC FUNCTION MySleep(dwMilliseconds AS DWORD) AS VOID PASCAL
RETURN 

// _DLL FUNC Sleep( dwMilliseconds AS DWORD) AS VOID PASCAL:KERNEL32.Sleep

FUNCTION test() AS INT
    STATIC pFunc := NULL AS MySleep PTR // <- this does not compile. The generated delegate does not see dosomething()
    IF pFunc == NULL
         VAR hDll := LoadLibrary("Kernel32.DLL")
         xAssert(hDLL != NULL_PTR)
         pFunc := GetProcAddress(hDLL, "Sleep")
         xAssert(pFunc != NULL_PTR)
    ENDIF
    PCall(pFunc, 100)  
    PCall(g,100)
    RETURN 1



_DLL FUNC LoadLibrary(lpLibFileName AS STRING) AS PTR PASCAL:KERNEL32.LoadLibraryA ANSI
_DLL FUNC GetProcAddress(hModule AS PTR, lpProcName AS STRING) AS PTR PASCAL:KERNEL32.GetProcAddress ANSI
    
    
PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN   

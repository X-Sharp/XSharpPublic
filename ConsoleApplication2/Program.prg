USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start AS VOID
	
LOCAL p AS PSZ
LOCAL i AS DWORD

p := MemAlloc(100)
MemCopyString(p, "abc", 3) // ok
i := 3
MemCopyString(p + i, "de", 2) // it gives an incorrect result
? p

MemCopyString(p + 3, "de", 2) // it gives an incorrect result also
? p
 
MemFree(p)               

WAIT
RETURN

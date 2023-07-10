// 22. compiler crash
FUNCTION Test123(a AS INT[]) AS VOID
LOCAL n AS INT
FOR n := 1 UPTO a:Length
   NOP
NEXT

FUNCTION Start() AS VOID

RETURN

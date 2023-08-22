// 105. error XS0103: The name 'n' does not exist in the current context
// used to be compile crash, bug 17
FUNCTION Start() AS VOID
FOR LOCAL n := 1 AS INT UPTO 10
   NOP

NEXT


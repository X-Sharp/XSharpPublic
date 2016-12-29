// C366. DEFINEs need type specified
DEFINE ccOptimistic := 1
DEFINE ccNone := 0

FUNCTION Start() AS VOID
IntTest(ccOptimistic)
DWordTest(ccOptimistic)
UsualTest(ccNone)

Overloaded(ccOptimistic)
Overloaded(ccNone)
RETURN

FUNCTION IntTest(n AS INT) AS VOID
? n
FUNCTION DWordTest(n AS DWORD) AS VOID
? n
FUNCTION UsualTest(n AS USUAL) AS VOID
? n

FUNCTION Overloaded(n AS INT) AS VOID
? "int"
FUNCTION Overloaded(n AS DWORD) AS VOID
? "dword"
FUNCTION Overloaded(n AS USUAL) AS VOID
? "usual"

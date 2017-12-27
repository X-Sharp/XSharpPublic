// C366. DEFINEs need type specified
DEFINE ccNone 			:= -1
DEFINE ccOptimistic 	:= ccNone+1
DEFINE ccPesstimistic 	:= ccOptimistic+1
DEFINE dToday := Today()
DEFINE dBirthday := 1959.06.08   
DEFINE dBirthday2 := SToD("20030219")
DEFINE pSomething := PTR(_CAST, 0x12345678)
DEFINE pSomething2 := (PTR) 0x12345678
DEFINE Name := "Robert"
DEFINE Name2 := e"Robert\tvan\tder\tHulst"
DEFINE Sym  := #SomeSymbol
DEFINE Pi   := 3.14
DEFINE NOTHING := {}
DEFINE NOTS := NIL                   
STATIC DEFINE istrue := TRUE

DEFINE NewDefine := istrue
GLOBAL NewGlobal := istrue AS LOGIC

DEFINE foo  :=  Left(name,1) AS STRING
DEFINE bar  :=  iif( !istrue, foo,  "L")

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

// 741. Problems passing strongly typed vars by reference with the @ operator in late bound calls
// /vo7+
/*
Following code reports those errors. Note that all error line numbers point to the beginning of the Start() function, 
instead of the actual line with the (bogus) error
C741.prg(14,1): error XS0029: Cannot implicitly convert type 'string' to 'System.IntPtr*'
C741.prg(14,1): error XS0029: Cannot implicitly convert type 'logic' to 'logic*'
C741.prg(14,1): error XS0029: Cannot implicitly convert type 'DATE' to 'XSharp.__Date*'
C741.prg(14,1): error XS0029: Cannot implicitly convert type 'FLOAT' to 'XSharp.__Float*'
C741.prg(14,1): error XS0029: Cannot implicitly convert type 'real8' to 'real8*'
C741.prg(14,1): error XS0029: Cannot implicitly convert type 'real4' to 'real4*'
C741.prg(14,1): error XS0029: Cannot implicitly convert type 'SYMBOL' to 'XSharp.__Symbol*'
*/
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := TestClass{}

LOCAL l AS LOGIC
l := TRUE
u:TestMethod(REF l)
? l
xAssert(l == FALSE)

l := TRUE
u:TestMethod(@l)
? l
xAssert(l == FALSE)


LOCAL i AS INT
i := 555
u:TestMethod(REF i)
? i
xAssert(i == 123)

i := 555
u:TestMethod(@i)
? i
xAssert(i == 123)


LOCAL c AS STRING
c := "asd"
u:TestMethod(REF c)
? c
xAssert(c == "test")

c := "asd"
u:TestMethod(@c)
? c
xAssert(c == "test")


LOCAL d AS DATE
d := Today()
u:TestMethod(REF d)
? d

xAssert(d == Today()-1)
d := Today()
u:TestMethod(@d)
? d
xAssert(d == Today()-1)



LOCAL f AS FLOAT
u:TestMethod(@f)
LOCAL r8 AS REAL8
u:TestMethod(@r8)
LOCAL r4 AS REAL4
u:TestMethod(@r4)
LOCAL uu AS USUAL
u:TestMethod(@uu)
LOCAL s := #ASD AS SYMBOL
u:TestMethod(@s)

RETURN

CLASS TestClass
	METHOD TestMethod(u)
		DO CASE
		CASE IsString(u)
			u := "test"
		CASE IsNumeric(u)
			u := 123
		CASE IsDate(u)
			u := Today() - 1
		CASE IsLogic(u)
			u := FALSE
		END CASE
	RETURN NIL
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


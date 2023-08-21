// R750 ByRef dword passed to Clipper Calling Convention
FUNCTION Start() AS VOID STRICT    
    LOCAL iRow AS DWORD
    
    iRow := 100
    ? iRow   
    
    Header(@iRow)   
    xAssert(iRow == 0)
    
    ? iRow
    
    LOCAL i AS INT
    LOCAL w AS DWORD
    LOCAL b AS BYTE
    LOCAL c AS STRING
    LOCAL l AS LOGIC
    LOCAL d,d2 := NULL_DATE AS DATE
    BiggerTest(@i,@w,@b,@c,@l,@d,d2)
    xAssert(i == 1)
    xAssert(w == 2)
    xAssert(b == 3)
    xAssert(c == "4")
    xAssert(l == TRUE)
    xAssert(d == Today())
    xAssert(d2 == NULL_DATE)

    LOCAL ui AS USUAL
    LOCAL uw AS USUAL
    LOCAL ub AS USUAL
    LOCAL uc AS USUAL
    LOCAL ul AS USUAL
    LOCAL ud,ud2 := NULL_DATE AS DATE
    BiggerTest(@ui,@uw,@ub,@uc,@ul,@ud,ud2)
    xAssert(ui == 1)
    xAssert(uw == 2)
    xAssert(ub == 3)
    xAssert(uc == "4")
    xAssert(ul == TRUE)
    xAssert(ud == Today())
    xAssert(ud2 == NULL_DATE)
    
RETURN	
   
FUNCTION Header(iRow)
    ? iRow
    xAssert(iRow == 100)    
    iRow := 0
    ? iRow
    
    RETURN NIL

FUNCTION BiggerTest(i,w,b,c,l,d,d2)
i := 1
w := 2
b := 3
c := "4"
l := TRUE
d := Today()
d2 := Today()
    RETURN NIL
    

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"    

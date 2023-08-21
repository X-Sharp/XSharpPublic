//#pragma options("vo4", on)
//#pragma options("vo11", on)
//FUNCTION Start( ) AS VOID
//local b as Byte
//b := 10
//b  := _Or(b,TO_ADVANCEDTYPOGRAPHY)
//? b
//b >>= TO_ADVANCEDTYPOGRAPHY
//? b
//b  += TO_ADVANCEDTYPOGRAPHY
//? b
//b  -= TO_ADVANCEDTYPOGRAPHY
//? b
//b  :=  b + TO_ADVANCEDTYPOGRAPHY
//? b
//b  :=  b - TO_ADVANCEDTYPOGRAPHY
//? b
//? b
//testfractional()
//RETURN
//
//function test as int
//LOCAL b := 1 AS BYTE
//LOCAL n := 2 AS INT
//local dw := 2u as dword
//LOCAL w := 2 AS word
//b := _Or(b,n)
//? b
//b := _Or(b, w)
//? b
//
//b |= w
//? b
//n := dw
//b := 1.0
//? b
//local d as Decimal
//d := 1.0m
//d := (Decimal) 1.0
//? d
//return b
//
//
//
//function testfractional() as void
//    local f as float
//    local r8 as REAL8
//    local r4 as REAL4
//    local d as Decimal
//    local c as currency
//    f := r8 := r4 := 1.5
//    d := c := 1.5m
//    TestMe(f)
//    TestMe(r8)
//    TestMe(r4)
//    TestMe(d)
//    TestMe(c)
//
//
//function TestMe(i as int) as int
//    ? i
//    return i


#pragma options("vo11", on)
#pragma options("vo4", on)
function Start() as void
local f as float
local r8 as real8
local i as Long
r8 := f := 1.5
i := f            // this converts the float to real8 and then does the assignment
? i, f
i := r8
? i, r8
return

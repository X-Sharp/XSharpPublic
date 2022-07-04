#pragma options("vo4", on)
#pragma options("vo11", on)
FUNCTION Start( ) AS VOID
local b as Byte
b := 10
b  := _Or(b,TO_ADVANCEDTYPOGRAPHY)
? b
b >>= TO_ADVANCEDTYPOGRAPHY
? b
b  += TO_ADVANCEDTYPOGRAPHY
? b
b  -= TO_ADVANCEDTYPOGRAPHY
? b
b  :=  b + TO_ADVANCEDTYPOGRAPHY
? b
b  :=  b - TO_ADVANCEDTYPOGRAPHY
? b
? b

RETURN

function test as int
LOCAL b := 1 AS BYTE
LOCAL n := 2 AS INT
local dw := 2u as dword
LOCAL w := 2 AS word
b := _Or(b,n)
? b
b := _Or(b, w)
? b

b |= w
? b
n := dw
b := 1.0
? b
local d as Decimal
d := 1.0m
d := (Decimal) 1.0
? d
return b



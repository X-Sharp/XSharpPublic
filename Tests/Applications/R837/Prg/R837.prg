// /vo4 always off, try with /vo7+ and /vo7-     
#pragma options("vo4", on)      // signed - unsigned same size
#pragma options("vo11", on)     // conversions between different sizes and from fractional to integral
FUNCTION Start() AS VOID
LOCAL d := 0 AS DWORD
LOCAL i := 0 AS INT          
local b := 0 as byte
local f := 0.0 as float
local aArray := {} as ARRAY
d := i                          // warning XS9021 expected (Signed / Unsigned)
i := d                          // warning XS9021 expected (Signed / Unsigned)
i := f                          // warning XS9020 expected (Narrowing conversion)
d := f                          // warning XS9020 expected (Narrowing conversion)
b := i                          // warning XS9020 expected (Narrowing conversion)
i := b                          // silent
////          
? testme(i)

b := BYTE(i)                    // silent
b := BYTE(_CAST,i)              // silent
b := (BYTE) i                   // silent

? ALen(aArray) > 0

? b


Function TestMe(b as byte) as Byte
    return b

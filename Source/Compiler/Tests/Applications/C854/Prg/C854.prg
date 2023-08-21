// 854. Incorrect signed/unsigned conversion warning with numeric shift operators and DWORD operands
// /vo4+ /warnings as errors
FUNCTION Start( ) AS VOID
LOCAL dwVal := 1 AS DWORD
LOCAL dwBit := 1 AS DWORD
LOCAL o AS OBJECT
// warning XS9021: Signed/unsigned conversions from 'dword' to 'int' may lead to loss of data or overflow errors
// (in all lines)
dwVal := _OR( dwVal, 1u << dwBit )
dwVal := 1u << dwBit
dwVal := dwVal << dwBit
? dwVal << dwBit
o := dwVal << dwBit
? o

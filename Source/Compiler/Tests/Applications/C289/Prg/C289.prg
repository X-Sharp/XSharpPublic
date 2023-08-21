// 289. error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('byte[]')
// using /vo7 (which shouln'd be needed), the code compiles without errors, but prints
// random values, instead of 128 and 255

FUNCTION Start() AS VOID
LOCAL DIM bErrMsg[2] AS BYTE
LOCAL bp AS BYTE PTR
bErrMsg[1] := 128
bErrMsg[2] := 255
bp := (BYTE PTR)@bErrMsg
? BYTE(bp)
bp := (BYTE PTR)@bErrMsg[2]
? BYTE(bp) 


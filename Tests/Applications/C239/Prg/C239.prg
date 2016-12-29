// 239. error XS7038: Failed to emit module 'x'.
// found in user code, probably "@" was a leftover (?)
FUNCTION Start() AS VOID
LOCAL DIM abTemp[256] AS BYTE
@abTemp[1] := 255
? @abTemp[1]


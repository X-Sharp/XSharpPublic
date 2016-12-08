// 328. error XS0030: Cannot convert type 'Vulcan.__Usual' to 'byte[]'
FUNCTION Start( ) AS VOID
LOCAL u AS USUAL
LOCAL ab AS BYTE[]
u := BYTE[]{3}
? u
ab := (BYTE[])u
? ab


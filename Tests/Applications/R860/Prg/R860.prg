#pragma options("vo4", on)
FUNCTION Start() AS VOID
LOCAL dw:= 0  AS DWORD
LOCAL i := 0 AS INT

testD(i) // NO WARNING
testI(dw) // NO WARNING
testB(i) // NO WARNING
testB(dw) // NO WARNING

FUNCTION testI(n AS INT) AS VOID
FUNCTION testD(n AS DWORD) AS VOID
FUNCTION testB(n AS BYTE) AS VOID


FUNCTION TestRect as void
    local rect IS _winRect
    local dw := 0 as DWORD
    local nShrinkY := 1.0 as float
    rect.right := rect.left := 0
    rect.left += LONGINT(dw)
    rect.right := INT(FLOAT(rect:Right) * nShrinkY)


VOSTRUCT _winRECT
	MEMBER left AS LONGINT
	MEMBER top AS LONGINT
	MEMBER right AS LONGINT
	MEMBER bottom AS LONGINT

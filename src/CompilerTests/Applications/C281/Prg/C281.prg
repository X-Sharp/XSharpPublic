// 281. error XS0034: Operator '<=' is ambiguous on operands of type 'byte' and 'int'
// vulcan dialect, no matter if /vo4 is enabled or not
// note that all the following appear many times in user/SDK code and
// compile in vulcan without warnings on conversions (which makes sense I think)
// Also vulcan does not require /vo4 for the following to compile
#pragma warnings(165, off) // unassigned local
#pragma options("vo4", on)
FUNCTION Start() AS VOID
LOCAL d AS DWORD
LOCAL w AS WORD
LOCAL b AS BYTE
FOR b := 1 UPTO 10
	NOP
NEXT
FOR w := 1 UPTO 10
	NOP
NEXT
FOR b := 1 UPTO w
	NOP
NEXT
FOR b := 1 UPTO d
	NOP
NEXT
IF w == 1
	NOP
END IF
IF b = 1
	NOP
END IF
d := w + 1
d := b + 1
w := b + 1


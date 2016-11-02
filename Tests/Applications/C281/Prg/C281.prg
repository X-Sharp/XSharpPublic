// 281. error XS0034: Operator '<=' is ambiguous on operands of type 'byte' and 'int'
// vulcan dialect, no matter if /vo4 is enabled or not
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


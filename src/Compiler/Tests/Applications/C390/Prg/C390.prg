// 390. error XS9027: VoStruct member type must be one of the following: bool, byte, short, int, long, char, sbyte, ushort, uint, ulong, float, double, ptr, psz or vostruct
#pragma warnings(170, off) // unassigned field
VOSTRUCT myVOSTRUCT
MEMBER n AS INT
MEMBER sym AS SYMBOL
MEMBER ip AS IntPTR

UNION myUNION
MEMBER n AS INT
MEMBER sym AS SYMBOL
MEMBER ip AS IntPtr

FUNCTION Start() AS VOID
LOCAL v IS myVOSTRUCT
v:n := 1
v:sym := #TEST
v:ip := @v
? v:n , v:sym , v:ip
IF v:sym != #TEST
	THROW Exception{"Incorrect result"}
END IF
IF v:ip != @v
	THROW Exception{"Incorrect result"}
END IF

LOCAL u IS myUNION
u:sym := #ATEST
? u:n , u:sym


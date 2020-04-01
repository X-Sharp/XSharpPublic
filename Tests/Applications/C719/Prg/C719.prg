// 719. VO incompatibilities with numeric operations
/*
VO and X# give very different results in arithmetic operations, vo11+ should be used to emulate them
Below is a comprehensive list of tests, with the expected results taken from VO
Probbaly no need to emulate the behavior for all, but at least most of them should (compile and) give the same reslts in X#

This is the table of type results I have found VO to be making for arithmetic operations:

LHS		RHS		= RESULT
-------------------------
BYTE	BYTE	= BYTE
BYTE	WORD	= WORD
BYTE	SHORT	= SHORT
BYTE	DWORD	= DWORD
BYTE	INT		= INT

WORD	BYTE	= DWORD (or INT?)
WORD	WORD	= WORD
WORD	SHORT	= SHORT
WORD	DWORD	= DWORD
WORD	INT		= INT

SHORT	BYTE	= SHORT
SHORT	WORD	= SHORT
SHORT	SHORT	= SHORT
SHORT	DWORD	= DWORD
SHORT	INT		= INT

INT		BYTE	= INT
INT		WORD	= INT
INT		SHORT	= INT
INT		DWORD	= INT
INT		INT		= INT

DWORD	BYTE	= DWORD
DWORD	WORD	= DWORD
DWORD	SHORT	= DWORD
DWORD	DWORD	= DWORD
DWORD	INT		= DWORD

*/
FUNCTION Start() AS VOID
	? AsHexString( BYTE(0xFF) + BYTE(0xFF)         ) ; xAssert(BYTE(0xFF) + BYTE(0xFF)          ==      0x00FE)
	? AsHexString( BYTE(0xFF) + WORD(0xFF00)       ) ; xAssert(BYTE(0xFF) + WORD(0xFF00)        ==      0xFFFF)
	? AsHexString( BYTE(0xFF) + WORD(0xFFFF)       ) ; xAssert(BYTE(0xFF) + WORD(0xFFFF)        ==     0x100FE)
	? AsHexString( BYTE(0xFF) + SHORT(0x7F00)      ) ; xAssert(BYTE(0xFF) + SHORT(0x7F00)       ==     0x7FFFF)
	? AsHexString( BYTE(0xFF) + SHORT(0x7FFF)      ) ; xAssert(BYTE(0xFF) + SHORT(0x7FFF)       ==  0xFFFF80FE)
	? AsHexString( BYTE(0xFF) + DWORD(0x7FFFFFFF)  ) ; xAssert(BYTE(0xFF) + DWORD(0x7FFFFFFF)   ==  0x800000FE)
	? AsHexString( BYTE(0xFF) + DWORD(0xFFFFFFFFU) ) ; xAssert(BYTE(0xFF) + DWORD(0xFFFFFFFFU)  ==        0xFE)
	? AsHexString( BYTE(0xFF) + INT(0x7FFF0000)    ) ; xAssert(BYTE(0xFF) + INT(0x7FFF0000)     ==  0x7FFF00FF)
	? AsHexString( BYTE(0xFF) + INT(0x7FFFFFFF)    ) ; xAssert(BYTE(0xFF) + INT(0x7FFFFFFF)     ==  0x800000FE)

	? AsHexString( WORD(0xFF00) + BYTE(0xFF)         ) ; xAssert( WORD(0xFF00) + BYTE(0xFF)         = 0x0000FFFF )
	? AsHexString( WORD(0xFFFF) + BYTE(0xFF)         ) ; xAssert( WORD(0xFFFF) + BYTE(0xFF)         = 0x000100FE )
	? AsHexString( WORD(0xFFFF) + WORD(0xFFFF)       ) ; xAssert( WORD(0xFFFF) + WORD(0xFFFF)       = 0x0001FFFE )
	? AsHexString( WORD(0x8000) + SHORT(0x7FFF)      ) ; xAssert( WORD(0x8000) + SHORT(0x7FFF)      = 0xFFFFFFFF )
	? AsHexString( WORD(0xFFFF) + SHORT(0x7FFF)      ) ; xAssert( WORD(0xFFFF) + SHORT(0x7FFF)      = 0x00007FFE )
	? AsHexString( WORD(0xFFFF) + DWORD(0xFFFF0000)  ) ; xAssert( WORD(0xFFFF) + DWORD(0xFFFF0000)  = 0xFFFFFFFF )
	? AsHexString( WORD(0xFFFF) + DWORD(0xFFFFFFFF)  ) ; xAssert( WORD(0xFFFF) + DWORD(0xFFFFFFFF)  = 0x0000FFFE )
	? AsHexString( WORD(0xFFFF) + INT(0x7FFF0000)    ) ; xAssert( WORD(0xFFFF) + INT(0x7FFF0000)    = 0x7FFFFFFF )
	? AsHexString( WORD(0xFFFF) + INT(0x7FFFFFFF)    ) ; xAssert( WORD(0xFFFF) + INT(0x7FFFFFFF)    = 0x8000FFFE )

	? AsHexString( SHORT(0x7F00) + BYTE(0xFF)         ) ; xAssert( SHORT(0x7F00) + BYTE(0xFF)         == 0x00007FFF )
	? AsHexString( SHORT(0x7FFF) + BYTE(0xFF)         ) ; xAssert( SHORT(0x7FFF) + BYTE(0xFF)         == 0xFFFF80FE )
	? AsHexString( SHORT(0x7FFF) + WORD(0x8000)       ) ; xAssert( SHORT(0x7FFF) + WORD(0x8000)       == 0xFFFFFFFF )
	? AsHexString( SHORT(0x8000) + SHORT(0x7FFF)      ) ; xAssert( SHORT(0x8000) + SHORT(0x7FFF)      == 0xFFFFFFFF )
	? AsHexString( SHORT(0xFFFF) + SHORT(0xFFFF)      ) ; xAssert( SHORT(0xFFFF) + SHORT(0xFFFF)      == 0xFFFFFFFE )
	? AsHexString( SHORT(0x7FFF) + DWORD(0xFFFF0000)  ) ; xAssert( SHORT(0x7FFF) + DWORD(0xFFFF0000)  == 0xFFFF7FFF )
	? AsHexString( SHORT(0x7FFF) + DWORD(0xFFFFFFFF)  ) ; xAssert( SHORT(0x7FFF) + DWORD(0xFFFFFFFF)  == 0x00007FFE )
	? AsHexString( SHORT(0x7FFF) + INT(0x7FFF0000)    ) ; xAssert( SHORT(0x7FFF) + INT(0x7FFF0000)    == 0x7FFF7FFF )
	? AsHexString( SHORT(0x7FFF) + INT(0x7FFFFFFF)    ) ; xAssert( SHORT(0x7FFF) + INT(0x7FFFFFFF)    == 0x80007FFE )

	? AsHexString( INT(0x7FFFFF00) + BYTE(0xFF)         ) ; xAssert( INT(0x7FFFFF00) + BYTE(0xFF)         = 0x7FFFFFFF )
	? AsHexString( INT(0x7FFFFFFF) + BYTE(0xFF)         ) ; xAssert( INT(0x7FFFFFFF) + BYTE(0xFF)         = 0x800000FE )
	? AsHexString( INT(0x7FFF0000) + WORD(0xFFFF)       ) ; xAssert( INT(0x7FFF0000) + WORD(0xFFFF)       = 0x7FFFFFFF )
	? AsHexString( INT(0x7FFFFFFF) + WORD(0xFFFF)       ) ; xAssert( INT(0x7FFFFFFF) + WORD(0xFFFF)       = 0x8000FFFE )
	? AsHexString( INT(0x7FFF0000) + SHORT(0x7FFF)      ) ; xAssert( INT(0x7FFF0000) + SHORT(0x7FFF)      = 0x7FFF7FFF )
	? AsHexString( INT(0xFFFFFFFF) + SHORT(0xFFFF)      ) ; xAssert( INT(0xFFFFFFFF) + SHORT(0xFFFF)      = 0xFFFFFFFE )
	? AsHexString( INT(0x7FFFFFFF) + DWORD(0x7FFFFFFF)  ) ; xAssert( INT(0x7FFFFFFF) + DWORD(0x7FFFFFFF)  = 0xFFFFFFFE )
	? AsHexString( INT(0xFFFFFFFF) + DWORD(0xFFFFFFFF)  ) ; xAssert( INT(0xFFFFFFFF) + DWORD(0xFFFFFFFF)  = 0xFFFFFFFE )
	? AsHexString( INT(0x7FFFFFFF) + INT(0x7FFFFFFF)    ) ; xAssert( INT(0x7FFFFFFF) + INT(0x7FFFFFFF)    = 0xFFFFFFFE )
	
	? AsHexString( DWORD(0x7FFFFF00) + BYTE(0xFF)         ) ; xAssert( DWORD(0x7FFFFF00) + BYTE(0xFF)         = 0x7FFFFFFF )
	? AsHexString( DWORD(0x7FFFFFFF) + BYTE(0xFF)         ) ; xAssert( DWORD(0x7FFFFFFF) + BYTE(0xFF)         = 0x800000FE )
	? AsHexString( DWORD(0x7FFF0000) + WORD(0xFFFF)       ) ; xAssert( DWORD(0x7FFF0000) + WORD(0xFFFF)       = 0x7FFFFFFF )
	? AsHexString( DWORD(0x7FFFFFFF) + WORD(0xFFFF)       ) ; xAssert( DWORD(0x7FFFFFFF) + WORD(0xFFFF)       = 0x8000FFFE )
	? AsHexString( DWORD(0x7FFF0000) + SHORT(0x7FFF)      ) ; xAssert( DWORD(0x7FFF0000) + SHORT(0x7FFF)      = 0x7FFF7FFF )
	? AsHexString( unchecked(DWORD(0xFFFFFFFF) + SHORT(0xFFFF))      ) ; xAssert( unchecked(DWORD(0xFFFFFFFF) + SHORT(0xFFFF) )     = 0xFFFFFFFE )
	? AsHexString( DWORD(0x7FFFFFFF) + DWORD(0x7FFFFFFF)  ) ; xAssert( DWORD(0x7FFFFFFF) + DWORD(0x7FFFFFFF)  = 0xFFFFFFFE )
	? AsHexString( DWORD(0xFFFFFFFF) + DWORD(0xFFFFFFFF)  ) ; xAssert( DWORD(0xFFFFFFFF) + DWORD(0xFFFFFFFF)  = 0xFFFFFFFE )
	? AsHexString( DWORD(0x7FFFFFFF) + INT(0x7FFFFFFF)    ) ; xAssert( DWORD(0x7FFFFFFF) + INT(0x7FFFFFFF)    = 0xFFFFFFFE )
RETURN

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		? "Assertion failed"
//		THROW Exception{"Incorrect result"}
	END IF   
RETURN

// 719. VO incompatibilities with numeric operations
/*
VO and X# give very different results in arithmetic operations, vo11+ should be used to emulate them
Below is a comprehensive list of tests, with the expected results taken from VO
Probbaly no need to emulate the behavior for all, but at least most of them should (compile and) give the same reslts in X#

This is the table of type results I have found VO to be making for arithmetic operations:

LHS		RHS		= RESULT
-------------------------
BYTE	BYTE	= BYTE       	 // Largest of the 2 types
BYTE	WORD	= WORD			 // When same size then LHS type
BYTE	SHORT	= SHORT
BYTE	DWORD	= DWORD
BYTE	INT		= INT

WORD	BYTE	= DWORD (or INT?)// largest of the 2 types. 
WORD	WORD	= WORD           // When same size then signed type
WORD	SHORT	= SHORT
WORD	DWORD	= DWORD
WORD	INT		= INT

SHORT	BYTE	= SHORT			 // largest of the 2 types. 
SHORT	WORD	= SHORT			 // When same size then LHS type
SHORT	SHORT	= SHORT
SHORT	DWORD	= DWORD
SHORT	INT		= INT

INT		BYTE	= INT			 // largest of the 2 types. 			
INT		WORD	= INT			 // When same size then LHS type
INT		SHORT	= INT
INT		DWORD	= INT
INT		INT		= INT

DWORD	BYTE	= DWORD			 // largest of the 2 types. 			
DWORD	WORD	= DWORD			 // When same size then LHS type
DWORD	SHORT	= DWORD
DWORD	DWORD	= DWORD
DWORD	INT		= DWORD

*/
FUNCTION Start() AS VOID                                                                                      
	BEGIN UNCHECKED 
	// please note that the results in the last column
	// for DWORD and INT results have been labeled with an U or L to make sure 
	// that it is of the right type. This is especially important for 8 digit hex numbers where the first
	// digit is > 7
	? "BB",AsHexString( BYTE(0xFF) + BYTE(0xFF)         ) ; xAssert(BYTE(0xFF) + BYTE(0xFF)          ==      0x00FE)
	? "BW",AsHexString( BYTE(0xFF) + WORD(0xFF00)       ) ; xAssert(BYTE(0xFF) + WORD(0xFF00)        ==      0xFFFF)
	? "BW",AsHexString( BYTE(0xFF) + WORD(0xFFFF)       ) ; xAssert(BYTE(0xFF) + WORD(0xFFFF)        ==     0x100FE) // fails, 5 digits ?
	? "BS",AsHexString( BYTE(0xFF) + SHORT(0x7F00)      ) ; xAssert(BYTE(0xFF) + SHORT(0x7F00)       ==     0x7FFFF) // fails, 5 digits ?
	? "BS",AsHexString( BYTE(0xFF) + SHORT(0x7FFF)      ) ; xAssert(BYTE(0xFF) + SHORT(0x7FFF)       ==  0xFFFF80FE)
	? "BD",AsHexString( BYTE(0xFF) + DWORD(0x7FFFFFFF)  ) ; xAssert(BYTE(0xFF) + DWORD(0x7FFFFFFF)   ==  0x800000FE)
	? "BD",AsHexString( BYTE(0xFF) + DWORD(0xFFFFFFFFU) ) ; xAssert(BYTE(0xFF) + DWORD(0xFFFFFFFFU)  ==        0xFE)
	? "BI",AsHexString( BYTE(0xFF) + INT(0x7FFF0000)    ) ; xAssert(BYTE(0xFF) + INT(0x7FFF0000)     ==  0x7FFF00FFL)
	? "BI",AsHexString( BYTE(0xFF) + INT(0x7FFFFFFF)    ) ; xAssert(BYTE(0xFF) + INT(0x7FFFFFFF)     ==  0x800000FEL)

	? "WB",AsHexString( WORD(0xFF00) + BYTE(0xFF)         ) ; xAssert( WORD(0xFF00) + BYTE(0xFF)         = 0x0000FFFF )
	? "WB",AsHexString( WORD(0xFFFF) + BYTE(0xFF)         ) ; xAssert( WORD(0xFFFF) + BYTE(0xFF)         = 0x000100FE )
	? "WW",AsHexString( WORD(0xFFFF) + WORD(0xFFFF)       ) ; xAssert( WORD(0xFFFF) + WORD(0xFFFF)       = 0x0001FFFE )   // fails, 5 digits ?
	? "WS",AsHexString( WORD(0x8000) + SHORT(0x7FFF)      ) ; xAssert( WORD(0x8000) + SHORT(0x7FFF)      = 0xFFFFFFFF )
	? "WS",AsHexString( WORD(0xFFFF) + SHORT(0x7FFF)      ) ; xAssert( WORD(0xFFFF) + SHORT(0x7FFF)      = 0x00007FFE )
	? "WD",AsHexString( WORD(0xFFFF) + DWORD(0xFFFF0000)  ) ; xAssert( WORD(0xFFFF) + DWORD(0xFFFF0000)  = 0xFFFFFFFFU )
	? "WD",AsHexString( WORD(0xFFFF) + DWORD(0xFFFFFFFF)  ) ; xAssert( WORD(0xFFFF) + DWORD(0xFFFFFFFF)  = 0x0000FFFEU )
	? "WI",AsHexString( WORD(0xFFFF) + INT(0x7FFF0000)    ) ; xAssert( WORD(0xFFFF) + INT(0x7FFF0000)    = 0x7FFFFFFFL )
	? "WI",AsHexString( WORD(0xFFFF) + INT(0x7FFFFFFF)    ) ; xAssert( WORD(0xFFFF) + INT(0x7FFFFFFF)    = 0x8000FFFEL )

	? "SB",AsHexString( SHORT(0x7F00) + BYTE(0xFF)         ) ; xAssert( SHORT(0x7F00) + BYTE(0xFF)         == 0x00007FFF )
	? "SB",AsHexString( SHORT(0x7FFF) + BYTE(0xFF)         ) ; xAssert( SHORT(0x7FFF) + BYTE(0xFF)         == 0xFFFF80FE )
	? "SW",AsHexString( SHORT(0x7FFF) + WORD(0x8000)       ) ; xAssert( SHORT(0x7FFF) + WORD(0x8000)       == 0xFFFFFFFF )
	? "SS",AsHexString( SHORT(0x8000) + SHORT(0x7FFF)      ) ; xAssert( SHORT(0x8000) + SHORT(0x7FFF)      == -1 )
	? "SS",AsHexString( SHORT(0xFFFF) + SHORT(0xFFFF)      ) ; xAssert( SHORT(0xFFFF) + SHORT(0xFFFF)      == -1 )
	? "SD",AsHexString( SHORT(0x7FFF) + DWORD(0xFFFF0000)  ) ; xAssert( SHORT(0x7FFF) + DWORD(0xFFFF0000)  == 0xFFFF7FFF )
	? "SD",AsHexString( SHORT(0x7FFF) + DWORD(0xFFFFFFFF)  ) ; xAssert( SHORT(0x7FFF) + DWORD(0xFFFFFFFF)  == 0x00007FFE )
	? "SI",AsHexString( SHORT(0x7FFF) + INT(0x7FFF0000)    ) ; xAssert( SHORT(0x7FFF) + INT(0x7FFF0000)    == 0x7FFF7FFF )
	? "SI",AsHexString( SHORT(0x7FFF) + INT(0x7FFFFFFF)    ) ; xAssert( SHORT(0x7FFF) + INT(0x7FFFFFFF)    == 0x80007FFEL )

	? "IB",AsHexString( INT(0x7FFFFF00) + BYTE(0xFF)         ) ; xAssert( INT(0x7FFFFF00) + BYTE(0xFF)         = 0x7FFFFFFFL )
	? "IB",AsHexString( INT(0x7FFFFFFF) + BYTE(0xFF)         ) ; xAssert( INT(0x7FFFFFFF) + BYTE(0xFF)         = 0x800000FEL )
	? "IW",AsHexString( INT(0x7FFF0000) + WORD(0xFFFF)       ) ; xAssert( INT(0x7FFF0000) + WORD(0xFFFF)       = 0x7FFFFFFFL )
	? "IS",AsHexString( INT(0x7FFFFFFF) + WORD(0xFFFF)       ) ; xAssert( INT(0x7FFFFFFF) + WORD(0xFFFF)       = 0x8000FFFEL )
	? "IS",AsHexString( INT(0x7FFF0000) + SHORT(0x7FFF)      ) ; xAssert( INT(0x7FFF0000) + SHORT(0x7FFF)      = 0x7FFF7FFFL )
	? "IS",AsHexString( INT(0xFFFFFFFF) + SHORT(0xFFFF)      ) ; xAssert( INT(0xFFFFFFFF) + SHORT(0xFFFF)      = 0xFFFFFFFEL )
	? "ID",AsHexString( INT(0x7FFFFFFF) + DWORD(0x7FFFFFFF)  ) ; xAssert( INT(0x7FFFFFFF) + DWORD(0x7FFFFFFF)  = 0xFFFFFFFEL )
	? "ID",AsHexString( INT(0xFFFFFFFF) + DWORD(0xFFFFFFFF)  ) ; xAssert( INT(0xFFFFFFFF) + DWORD(0xFFFFFFFF)  = 0xFFFFFFFEL )
	? "II",AsHexString( INT(0x7FFFFFFF) + INT(0x7FFFFFFF)    ) ; xAssert( INT(0x7FFFFFFF) + INT(0x7FFFFFFF)    = 0xFFFFFFFEL )
	
	? "DB",AsHexString( DWORD(0x7FFFFF00) + BYTE(0xFF)         ) ; xAssert( DWORD(0x7FFFFF00) + BYTE(0xFF)         = 0x7FFFFFFF )
	? "DB",AsHexString( DWORD(0x7FFFFFFF) + BYTE(0xFF)         ) ; xAssert( DWORD(0x7FFFFFFF) + BYTE(0xFF)         = 0x800000FE )
	? "DW",AsHexString( DWORD(0x7FFF0000) + WORD(0xFFFF)       ) ; xAssert( DWORD(0x7FFF0000) + WORD(0xFFFF)       = 0x7FFFFFFF )
	? "DW",AsHexString( DWORD(0x7FFFFFFF) + WORD(0xFFFF)       ) ; xAssert( DWORD(0x7FFFFFFF) + WORD(0xFFFF)       = 0x8000FFFE )
	? "DS",AsHexString( DWORD(0x7FFF0000) + SHORT(0x7FFF)      ) ; xAssert( DWORD(0x7FFF0000) + SHORT(0x7FFF)      = 0x7FFF7FFF )
	? "DS",AsHexString( unchecked(DWORD(0xFFFFFFFF) + SHORT(0xFFFF))      ) ; xAssert( unchecked(DWORD(0xFFFFFFFF) + SHORT(0xFFFF) )     = 0xFFFFFFFE )
	? "DD",AsHexString( DWORD(0x7FFFFFFF) + DWORD(0x7FFFFFFF)  ) ; xAssert( DWORD(0x7FFFFFFF) + DWORD(0x7FFFFFFF)  = 0xFFFFFFFE )
	? "DD",AsHexString( DWORD(0xFFFFFFFF) + DWORD(0xFFFFFFFF)  ) ; xAssert( DWORD(0xFFFFFFFF) + DWORD(0xFFFFFFFF)  = 0xFFFFFFFE )
	? "DI",AsHexString( DWORD(0x7FFFFFFF) + INT(0x7FFFFFFF)    ) ; xAssert( DWORD(0x7FFFFFFF) + INT(0x7FFFFFFF)    = 0xFFFFFFFE )
	END UNCHECKED
RETURN

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		NOP // ? "assertion passed"
	ELSE
		? "Assertion failed"
//		THROW Exception{"Incorrect result"}
	END IF   
RETURN

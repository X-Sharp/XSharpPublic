// 717. error XS0266: Cannot implicitly convert type 'int' to 'byte'. An explicit conversion exists (are you missing a cast?)
FUNCTION Start() AS VOID
	xAssert( GetByte1(1) == 3)
	xAssert( GetByte1(255) == 1)
	xAssert( GetByte2(1) == 3)
	xAssert( GetByte2(255) == 1)
	xAssert( GetByte3(1) == 3)
	xAssert( GetByte3(255) == 1)
RETURN

FUNCTION GetByte1(b AS BYTE) AS BYTE
	b := b + 1 			// OK
	b := b + (BYTE)1	// OK
RETURN b				// OK

FUNCTION GetByte2(b AS BYTE) AS BYTE
RETURN b + 2 			// error XS0266

FUNCTION GetByte3(b AS BYTE) AS BYTE
RETURN b + (BYTE)2 		// error XS0266


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF   
RETURN

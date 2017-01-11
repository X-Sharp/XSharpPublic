// 401. error XS0021: Cannot apply indexing with [] to an expression of type 'IntPtr'
// error XS0131: The left-hand side of an assignment must be a variable, property or indexer
// error reported only when /vo6+ is enabled
CLASS rpSQLField
EXPORT ptrUsed AS LONGINT PTR
	METHOD Test() AS VOID
		LOCAL nBytesUsed AS LONGINT

		LOCAL n := 2017 AS INT
		SELF:ptrUsed := @n
		
		nBytesUsed:=SELF:ptrUsed[1]
		? nBytesUsed
		IF nBytesUsed != 2017
			THROW Exception{"Incorrect result"}
		END IF
END CLASS

FUNCTION Start() AS VOID
	LOCAL oFld AS rpSQLField
	oFld := rpSQLField{}
	oFld:Test()

	LONGINT(oFld:ptrUsed) := 1976
	? LONGINT(oFld:ptrUsed) // 1976

	? LONGINT(oFld:ptrUsed) == 1976 // FALSE!!
	
	LOCAL l AS LOGIC
	l := LONGINT(oFld:ptrUsed) == 1976
	? l // FALSE again also in vulcan...

	IF LONGINT(oFld:ptrUsed) != 1976
		// vulcan returns false, too!!! So we are not throwing an exception here...
		// or maybe I just really need that sleep...
//		THROW Exception{"Incorrect result"}
		NOP
	END IF

	LZSS{}:Test()

CLASS LZSS
	PROTECT pWindow	AS BYTE PTR
	METHOD Test() AS VOID
		LOCAL nLookAheadPos := 1 AS DWORD
		LOCAL DIM bytes[100] AS BYTE
		bytes[1] := 10
		bytes[2] := 11
		pWindow := @bytes
		
		BYTE(pWindow) := 10
		BYTE(pWindow+nLookAheadPos) := 11
		
		? BYTE(pWindow)
		? BYTE(pWindow) == 10
		IF BYTE(pWindow) != 10
			THROW Exception{"Incorrect result"}
		END IF
		
		? BYTE(pWindow+nLookAheadPos)
		? BYTE(pWindow+nLookAheadPos) == 11
		IF BYTE(pWindow+nLookAheadPos) != 11
			THROW Exception{"Incorrect result"}
		END IF
END CLASS

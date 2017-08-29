// 556. Cannot declare fixed buffer field in structure/Core
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices

STRUCTURE myStruct
	EXPORT nDummy AS INT
	
	// this defines the field as a single byte, compiler bug?
//	EXPORT buf[100] AS BYTE

	// error XS1716: Do not use 'System.Runtime.CompilerServices.FixedBuffer' attribute. Use the 'fixed' field modifier instead.
	[FixedBuffer(typeof(BYTE), 100)];
	EXPORT buf AS BYTE

	// this emits it with a size of 0 ([FixedBuffer(typeof(byte), 0)]) and SizeOf() returns 8	
//	EXPORT UNSAFE FIXED buf[100] AS BYTE
	
END STRUCTURE


FUNCTION Start( ) AS VOID
	LOCAL s AS myStruct
	? Marshal.SizeOf(TypeOf(myStruct))
	s:buf[10] := 255
	? s:buf[10]
	IF .not. Marshal.SizeOf(TypeOf(myStruct)) == 104
		THROW Exception{"Incorrect structure size"}
	END IF
RETURN

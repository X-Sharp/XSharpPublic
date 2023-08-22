// 397. error XS1686: Local 'nBytesUsed' or its members cannot have their address taken and be used inside an anonymous method or lambda expression
// probably (?) this will be automatically fixed when C391 will be fixed
FUNCTION Start() AS VOID
	TestClass{}:CopyDataBuffer2DBF()
RETURN

CLASS TestClass
	EXPORT nArea AS INT
	METHOD CopyDataBuffer2DBF() AS LOGIC  
		LOCAL oFld AS rpSQLField
		LOCAL nBytesUsed AS LONGINT
		
		LOCAL n AS INT
		n := 1976
		oFld := rpSQLField{}
		oFld:ptrUsed := @n
		
		MemCopy(@nBytesUsed,oFld:ptrUsed,_sizeof(LONGINT))
		
		? nBytesUsed
		
		IF nBytesUsed != 1976
			THROW Exception{"Incorrect result"}
		END IF

		LOCAL l := FALSE AS LOGIC
		IF l
			// ignore this, just needed to reproduce the error message
			(SELF:nArea)->(FieldPut(1,Mem2String(oFld:ptrValue,(DWORD)nBytesUsed)))
		END IF
	RETURN TRUE
END CLASS

CLASS rpSQLField
EXPORT ptrUsed AS PTR
EXPORT ptrValue AS PTR
END CLASS

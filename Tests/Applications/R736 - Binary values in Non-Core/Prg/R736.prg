// R 736 - Binary values in Non Core dialect
FUNCTION Start( ) AS VOID
	var b1 := 0h34563456
	var b2 := 0h34   // 4
	var b3 := 0h56
	? b1:ToString()	// 0h34563456
	xAssert(b1:ToString() == "0h34563456")
	? b2:ToString()	// 0h34
	xAssert(b2:ToString() == "0h34")
	? b3:ToString() 
	xAssert(b3:ToString() == "0h56")
	var b4 := b2 + b3   
	? b4:ToString() // 0h3456   	
	xAssert(b4:ToString() == "0h3456")	
	? b4:ToString("G") // 4V  (display as string)
	xAssert(b4:ToString("G") == "4V")	
	? "abc"+b1		// ABC4V4V     
	xAssert("abc"+b1 == "abc4V4V")	

	? "def"+b2		// def4
	xAssert("def"+b2 == "def4")	
	? b2+"xyz"		// 4xyz   
	xAssert(b2+"xyz" == "4xyz")	
	
	? b3+"klm"      // Vklm      
	xAssert(b3+"klm" == "Vklm")	
	local u as usual
	u := b2
	? Valtype(u)	// Q
	xAssert(Valtype(u) == "Q")	
	? "a"+u		// a4
	xAssert("a"+u == "a4")	
	? u:ToString()      
	xAssert(u:ToString() == "0h34")	
	
	? usualType(u)	// 29
	xAssert(usualType(u) == 29)	
	u += u
	? u:ToString()      
	xAssert(u:ToString() == "0h3434")	
	
	local b := 0haeaeae as byte[]
	b1 :=  b
	? (BINARY) b1
	xAssert(b1:ToString() == "0hAEAEAE")	
	
	WAIT
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


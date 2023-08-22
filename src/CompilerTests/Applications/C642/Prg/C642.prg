// 642. Inconistncies in arithmetic between different numeric types
/*
This is a big mess also in VO. In VO, all arithmetic with BYTES give results with no 8 bit overflow, instead for example
BYTE(100) + WORD(1000) returns 1100. In vulcan and X#, the results with BYTE are inconsistent. Found this because of a
problem in the RDDs, where we were using (SHORT)nLen := (BYTE)header + (WORD)something_else, this was overflowing.

But when we have WORDs as the first operand, then the results are very inconsistent in VO, too, see below.
Really don't know what we should do with all this mess...
*/
FUNCTION Start() AS VOID
	LOCAL b AS BYTE
	LOCAL w AS WORD
	LOCAL s AS SHORT
	LOCAL n AS INT
	LOCAL u AS USUAL

// BYTE tests:
	? "BYTE Tests"
	? 
	b := 255
	n := 1
	
	u := b + n
	? u
	xAssert(u == 256)
	xAssertEquals(u , 256)
	
	u := b + 1
	? u
//	xAssert(u == 256)
//	xAssertEquals(u , 256)
	
	? b + 1
	xAssert(b + 1 == 256)
//	xAssertEquals(b + 1 , 256)

	? b + n
	xAssert(b + n == 256)
	xAssertEquals(b + n , 256)

	n := b + n
	? n
	xAssert(n == 256)
	xAssertEquals(n , 256)

	n := 1
	s := b + n
	? s
	xAssert(s == 256)
//	xAssertEquals(s , 256)
	



// WORD tests
	? "-----------"
	? "WORD Tests"
	?
	b := 1
	w := 65535
	? w + 1 // this prints 0 in VO. Argh!
	u := w + 1
	? u // 0
	
	n := w + 1
	? n // but this returns 65535 in VO!
	xAssert(n == 65536)
	xAssertEquals(n , 65536)

	n := w + b
	? n // also 65535
	xAssert(n == 65536)
	xAssertEquals(n , 65536)

	n := 1
	n := w + n
	? n // 65535
	xAssert(n == 65536)
	xAssertEquals(n , 65536)

	s := 1
	n := w + s
	? n // and this one gives 0 again! Grrrr!
	
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
PROC xAssertEquals(o1 AS USUAL, o2 AS USUAL)
? o1,o2
IF o1 == o2
	? "Assertion passed"
ELSE
	THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" , o2 , o1)}
END IF


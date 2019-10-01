// 687. Problems with undeclared vars/PUBLICs/PRIVATEs in VFP dialect

// error XS0161: 'C687.Exe.Functions.C687()': not all code paths return a value
PRIVATE i1
FUNCTION Test1( ) AS VOID
	FOR i1 = 1 UPTO 10
		? i1
	NEXT
RETURN

//error XS0131: The left-hand side of an assignment must be a variable, property or indexer
FUNCTION Test2( ) AS VOID
	PRIVATE i2
	FOR i2 = 1 UPTO 10
		? i2
	NEXT
RETURN

// System.InvalidProgramException at runtime
FUNCTION Test3( ) AS VOID
	FOR i3 = 1 UPTO 10
		? i3
	NEXT
RETURN


FUNCTION Start() AS VOID
	Test1()
	Test2()
	Test3()
RETURN


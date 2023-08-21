// 181. error XS0121: The call is ambiguous between the following methods or properties:
// 'TestClass.MyMethod(ref int)' and 'TestClass.MyMethod(ref double)'
// vulcan allows passing params by reference without specifying REF in the caller code,
// so that must be fully supported in x# as well, but I think it would be btter to make
// the compiler report a warning in such calls with missing REF
#pragma warnings(9071, off)   //  Parameter needs a(n) 'Out' modifier. This modifier was automatically added.
FUNCTION Start() AS VOID
TestClass{}:CallMethod()

CLASS TestClass
METHOD CallMethod() AS VOID
LOCAL nInt := 0 AS INT
LOCAL nDouble := 0 AS Double
LOCAL nDecimal := 0 AS Decimal

SELF:MyMethod( REF nInt ) // ok
SELF:MyMethod( nInt ) // error
SELF:MyMethod( nDouble ) // ok
SELF:MyMethod( nDecimal ) // ok

METHOD MyMethod( nInt REF INT ) AS VOID
	? __SIG__, nInt
METHOD MyMethod( nDouble REF Double ) AS VOID
		? __SIG__, nDouble
METHOD MyMethod( nDecimal REF Decimal ) AS VOID
		? __SIG__, nDecimal
END CLASS


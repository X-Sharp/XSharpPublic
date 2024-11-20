// 924. Problem using SizeOf() on a structure containing a string in the Core dialect
// https://github.com/X-Sharp/XSharpPublic/issues/1594
// works without errors in VO dialect
STRUCTURE MyStruct
	EXPORT n AS INT
	EXPORT c AS STRING
END STRUCTURE

FUNCTION Start() AS VOID
	? SizeOf(System.Drawing.Point) // OK
	? SizeOf(MyStruct) // error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('MyStruct')
RETURN

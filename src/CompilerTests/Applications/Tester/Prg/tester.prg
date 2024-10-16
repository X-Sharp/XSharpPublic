// Core dialect
PUBLIC STRUCTURE MyStruct
	EXPORT n AS INT
	EXPORT c AS STRING
END STRUCTURE

FUNCTION Start() AS VOID
	? SizeOf(System.Drawing.Point) // OK
	? SizeOf(MyStruct) // error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('MyStruct')
RETURN

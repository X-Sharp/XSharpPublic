// 524. error XS0117: 'File' does not contain a definition for 'Code'

#using System.IO

FUNCTION Start( ) AS VOID
	LOCAL aTest AS TestClass[]
	aTest := <TestClass>{TestClass{}}

	FOREACH file AS TestClass IN aTest
		? file:Code // ok

		LOCAL arr := {} AS ARRAY
		AAdd(arr , file:Code) // ok
		arr := {file:Code} // error XS0117
		AAdd(arr , file:Code) // ok
		AAdd(arr , {1, file:Code , 3}) // error XS0117
		? arr[1]
	NEXT
RETURN

CLASS TestClass
	PROPERTY Code AS STRING GET "code..."
END CLASS


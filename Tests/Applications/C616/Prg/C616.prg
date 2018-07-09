// 616. error XS0030: Cannot convert type 'void' to 'TestStruc'
/*
	PtrToStructure() has 4 overloads:

	STATIC METHOD PtrToStructure(@@ptr AS IntPtr, @@structure AS OBJECT) AS VOID
	STATIC METHOD PtrToStructure(@@ptr AS IntPtr, @@structure AS T) AS VOID
	STATIC METHOD PtrToStructure(@@ptr AS IntPtr, structureType AS Type) AS OBJECT
	STATIC METHOD PtrToStructure(@@ptr AS IntPtr) AS T
	
	When using 
	
	Marshal.PtrToStructure(p,TypeOf(TestStruc))
	
the compiler incorrectly picks the "OBJECT" overload, instead of the "Type" one.
*/

USING System.Runtime.InteropServices

CLASS Test
	STATIC METHOD TestOverload(o AS OBJECT) AS VOID
	STATIC METHOD TestOverload(o AS Type) AS INT
	RETURN 0
END CLASS


[StructLayout( LayoutKind.Sequential ) ];
STRUCTURE TestStruc
	EXPORT n AS INT
	EXPORT m AS INT
END STRUCTURE

FUNCTION Start() AS VOID
	LOCAL n AS INT
	n := Test.TestOverload(TypeOf(INT)) // Hmm, this one works
	? n

	LOCAL s AS TestStruc
	LOCAL p AS IntPtr
	s:n := 1
	s:m := 2
	p := Marshal.AllocHGlobal(Marshal.SizeOf(TypeOf(TestStruc)))
	Marshal.StructureToPtr(s,p,FALSE)
	
	s:n := 123
	s:m := 456
	
	s := (TestStruc) Marshal.PtrToStructure(p,TypeOf(TestStruc))
	? s:n , s:m
	xAssert(s:n == 1)
	xAssert(s:m == 2)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF


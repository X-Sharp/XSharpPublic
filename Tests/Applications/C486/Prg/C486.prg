// 486. error XS0121: The call is ambiguous between the following methods or properties: 'Functions.TestFunc(ref Child)' and 'Functions.TestFunc(ref Parent)'
FUNCTION Start() AS VOID
	LOCAL oTest AS Child
	oTest := Child{}
	TestFunc( REF oTest) // ok
	TestFunc( @oTest) // error XS0121
RETURN

FUNCTION TestFunc( o REF Child ) AS VOID STRICT 
FUNCTION TestFunc( o REF Parent ) AS VOID STRICT 

CLASS Parent
END CLASS

CLASS Child INHERIT Parent
END CLASS

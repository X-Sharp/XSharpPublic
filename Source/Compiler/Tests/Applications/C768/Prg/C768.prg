// 768. Problem with overriding PROPERTY from a different assembly
#pragma warnings(108, off) //   hides member
FUNCTION Start( ) AS VOID
RETURN

CLASS ParentInSame
	EXPORT fld AS INT
	ASSIGN Foo(n AS INT)
	SELF:fld := n
END CLASS

// This only reports a warning
CLASS ChildInSame INHERIT ParentInSame
	PROPERTY Foo AS INT GET 0 SET // XS0108: 'ChildInSame.Foo' hides inherited member 'ParentInSame.Foo'
END CLASS

// This is the exact same code, except that the parent class is defined in another assembly, but it reports an error in this case
// In previous compiler builds, it was actually not even reporting a warning either! (it should though)
CLASS ChildInLibrary INHERIT ParentInLibrary
	PROPERTY Foo AS INT GET 0 SET // error XS0545: 'ChildInLibrary.Foo.get': cannot override because 'ParentInLibrary.Foo' does not have an overridable get accessor
END CLASS

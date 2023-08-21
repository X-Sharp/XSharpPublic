// 195. error XS1540: Cannot access protected member 'ParentClass.ProtectedMethod()' via a qualifier of type 'ParentClass'; the qualifier must be of type 'ChildClass' (or derived from it)
// in vulcan it compiles without errors and runs as "expected" at runtime!
// not sure we must allow it in x#, but it is a vulcan incompatibility and there's code like that out there..
// VO also compiles it without errors or warnings, too, but throws an error at runtime "NO EXPORTED METHOD"
CLASS ParentClass
PROTECTED METHOD ProtectedMethod() AS VOID
? "protected method called" // gets called in vulcan
END CLASS

CLASS ChildClass INHERIT ParentClass
METHOD Test() AS VOID
	LOCAL o AS ParentClass
	o := ParentClass{}
	o:ProtectedMethod()
END CLASS

FUNCTION Start() AS VOID
	ChildClass{}:Test()	


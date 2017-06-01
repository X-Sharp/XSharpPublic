// 525. error XS0117: 'Obj' does not contain a definition for 'n'
/*
I am logging this because it was reported by a user, who said that 
this code compiled without errors in build 11, but started getting the
compiler errors in build 12. So for your consideration, for me it's fine
if this remains an error.
*/
USING MyNameSpace

// this was defined in a separate assembly in the original code
CLASS MyNameSpace.Obj
END CLASS

CLASS AnotherClass
EXPORT n AS INT
END CLASS

FUNCTION Start() AS VOID
LOCAL obj AS AnotherClass
obj := AnotherClass{}
obj.n := 1
obj.ToString()


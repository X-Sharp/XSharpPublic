// 605. No compiler error with using event assignment as return value
FUNCTION Start() AS VOID
	LOCAL o AS OBJECT
	
//	 the following correctly reports error XS0029: Cannot implicitly convert type 'void' to 'object'
//	o := System.AppDomain.CurrentDomain:AssemblyLoad += Handler
	
//	 the following compiles without errors though and produces as expected an InvalidProgramException at rutnime
//	 The debug version of the compiler produces an Assertion Failed: Using the return value of a void method.
	o := <VOID>{ System.AppDomain.CurrentDomain:AssemblyLoad += Handler }	
RETURN

FUNCTION Handler(sender AS OBJECT , args AS AssemblyLoadEventArgs) AS VOID
RETURN


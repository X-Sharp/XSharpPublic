//
// XSharp allows you to not only use the using statement to link to namespaces
// You can also link to a static class and call the methods in this class as if they are functions.
// The functions WriteLine and ReadKey() in the following code are actually resolved as System.Console.WriteLine()
// and System.Console.ReadKey()
// Finally there is also the BEGIN USING .. END USING construct which controls the lifetime of a variable
// At the end of the block the Variable will be automatically disposed.
USING System
USING STATIC System.Console

FUNCTION Start() AS VOID
    WriteLine("Hello World!")
    WriteLine("Press any key to continue...")
    WriteLine("Before Using Block")
	WriteLine("------------------")
	BEGIN USING VAR oTest := Test{}
		oTest:DoSomething()
	END USING 
	WriteLine("------------------")
    WriteLine("After Using Block")
	ReadKey()




CLASS Test IMPLEMENTS IDisposable
	CONSTRUCTOR()
		Console.WriteLine("Test:Constructor()")

	METHOD DoSomething() AS VOID
		Console.WriteLine("Test:DoSomething()")

	METHOD Dispose() AS VOID
		Console.WriteLine("Test:Dispose()")

END CLASS
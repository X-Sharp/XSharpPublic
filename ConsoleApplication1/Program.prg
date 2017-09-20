USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XSharp.Runtime.Tests
BEGIN NAMESPACE ConsoleApplication1

	FUNCTION Start() AS VOID
		SymbolTests{}:CreateSymbolTest()
        Console.ReadKey()
	
END NAMESPACE

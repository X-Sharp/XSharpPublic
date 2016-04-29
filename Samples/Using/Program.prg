//
// XSharp allows you to not only use the using statement to link to namespaces
// You can also link to a static class and call the methods in this class as if they are functions.
// The functions WriteLine and ReadKey() in the following code are actually resolved as System.Console.WriteLine()
// and System.Console.ReadKey()
using System
using static System.Console

Function Start() as void
    WriteLine("Hello World!")
    WriteLine("Press any key to continue...")
    ReadKey()
	


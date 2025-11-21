USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION Start() AS VOID STRICT
    Console.WriteLine("Hello World!")
	//
	VAR os := Environment.OSVersion
	Console.WriteLine("You are running on :")
	Console.WriteLine( "OS Version: " + os:Version:ToString() )
	Console.WriteLine( "OS Platform: " + os:Platform:ToString() )
    Console.WriteLine("Press any key to continue...")
    Console.ReadKey()




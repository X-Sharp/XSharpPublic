#using System
#using System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)#using System.Linq
$endif$#using System.Text

Begin Namespace $safeprojectname$

	Function Start() as void
        Console.WriteLine("Hello World!")
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()
	
End Namespace

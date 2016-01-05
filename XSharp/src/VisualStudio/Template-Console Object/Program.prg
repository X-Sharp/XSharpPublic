#using System
#using System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)#using System.Linq
$endif$#using System.Text

Begin Namespace $safeprojectname$
	Class Program
 
    static Method Main() as void
        Console.WriteLine("Hello World!")
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()
	
	
	End Class
End Namespace

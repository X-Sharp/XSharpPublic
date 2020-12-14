USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text

FUNCTION Start() AS VOID STRICT
    Console.WriteLine("Hello World!")
    Console.WriteLine("Press any key to continue...")
    Console.ReadKey()

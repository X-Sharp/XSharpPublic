
// Please read the comments in Readme.txt !
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID STRICT
    LOCAL cbMacro as CODEBLOCK
    LOCAL cMacro as STRING
    cMacro := "{||DTOC(Today())}"
    cbMacro := &(cMacro)
    Console.WriteLine("Hello World today is " + (STRING) Eval(cbMacro))
    
    Console.WriteLine("Press any key to continue...")
    Console.ReadKey()
    RETURN

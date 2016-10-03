// Please read the comments in Readme.txt !
USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text


FUNCTION Start() AS VOID
	LOCAL cbMacro as CODEBLOCK
	LOCAL cMacro as STRING
	VulcanLoader.InitVulcan() // required for VO/Vulcan database support
	cMacro := "{||DTOC(Today())}"
	cbMacro := &(cMacro)
        Console.WriteLine("Hello World today is " + (STRING) Eval(cbMacro))
		
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()
	
CLASS VulcanLoader
	STATIC METHOD InitVulcan() AS VOID
		// This method is needed if you want to use the Macro Compiler or the RDD system
		LOCAL t AS Type

		t := typeof(VulcanLoader)

		LOCAL mi AS System.Reflection.MethodInfo
		mi := t:GetMethod( "InitVulcan" )
		Vulcan.Runtime.State.AppModule := mi:Module
END CLASS
	

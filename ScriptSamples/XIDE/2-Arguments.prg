USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting

CLASS ScriptGlobals
	PUBLIC X AS REAL8
	PUBLIC Y AS REAL8

	PUBLIC METHOD Sqrt(n AS REAL8) AS REAL8
		RETURN Math.SqRt(n)
END CLASS

FUNCTION Start() AS VOID
	? "Passing arguments to script ..."
	XSharpScript.RunAsync(" ? 'X + Y = ', X+Y ", globals := ScriptGlobals{}{ X := 10, Y := 10 })
	?
	
	? "Making custom api functions accessible from the script ..."
	XSharpScript.RunAsync(" ? 'Sqrt( X^2 + Y^2 ) = ', Sqrt(X^2+Y^2) ", globals := ScriptGlobals{}{ X := 10, Y := 10 })
	?

	? "Core libraries are already available ..."
	XSharpScript.RunAsync(" ? 'Log10( X + Y ) = ', Math.Log10(X+Y) ", globals := ScriptGlobals{}{ X := 10, Y := 10 })
	?
	
	? "Class libraries can be loaded to provide an API ..."
	XSharpScript.RunAsync(" ? 'Distance( X , Y ) = ', Distance(X,Y) ", ;
		options := ScriptOptions.Default.WithReferences(typeof(ScriptApiType).Assembly), ;
		globals := ScriptGlobals{}{ X := 10, Y := 10 })
	?

	? "A script can modify global variables ..."
	VAR g := ScriptGlobals{}{ X := 10, Y := 10 }
	XSharpScript.RunAsync(" X := X + Y", globals := g)
	? "X =",g:X,"Y =",g:Y
	?

	? "Variables created by a script can be examined (useful for debugging) ..."
	VAR state := XSharpScript.RunAsync(" VAR Z := X + Y", globals := ScriptGlobals{}{ X := 10, Y := 10 }):Result
	? "Z =",state:GetVariable("Z").Value
	?

	RETURN



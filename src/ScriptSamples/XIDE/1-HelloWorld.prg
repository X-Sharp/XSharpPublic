USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting

FUNCTION Start() AS VOID
	? "Printing from within a script..."
	XSharpScript.RunAsync(" ? 'HELLO WORLD!' ")
	?
	
	? "The Print() function that pretty-prints stuff in xsi.exe is not available here..."
	TRY
		XSharpScript.RunAsync(" Print( 'HELLO WORLD!' ) ")
	CATCH e AS CompilationErrorException
		?
		Console.WriteLine("Caught exception: {0}",e:GetType())
	END TRY
	?

	? "A script could also return a value..."
	? XSharpScript.EvaluateAsync(" 1+1 "):Result
	?

	? "A script have multiple lines and still return a value..."
	? XSharpScript.EvaluateAsync(;
		e" LOCAL X := 10 AS INT \n"+;
		e" LOCAL Y := 10 AS INT \n"+;
		e" return X+Y";
		):Result
	?

	RETURN



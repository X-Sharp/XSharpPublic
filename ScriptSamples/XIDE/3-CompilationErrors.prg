USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting

FUNCTION Start() AS VOID
	// A script that contains a couple of errors (or more)
	VAR lines := <STRING>{;
		"VAR X := '1234'",;
		"VAR Y := 567",;
		"return X*Y",;
		"Print(X)";
		}

	// Join with newlines to get the script source	
	VAR source := String.Join(e"\r\n", lines)

	// Running the script causes an exception when the first compile-time error is identified
	Console.WriteLine("Trying to run a script with compile-time errors ...")
	Console.WriteLine()
	TRY
		XSharpScript.RunAsync(source)
	CATCH e AS CompilationErrorException
		Console.WriteLine("Exception thrown: {0}",e:GetType())
		Console.WriteLine("Exception message: {0}",e:Message)
	END TRY

	RETURN


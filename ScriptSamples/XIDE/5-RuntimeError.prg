USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting

FUNCTION Start() AS VOID
	// A script that causes a runtime error
	VAR lines := <STRING>{;
		"VAR X := 1234",;
		"VAR Y := 0",;
		"return X/Y";
		}

	// Join with newlines to get the script source	
	VAR source := String.Join(e"\r\n", lines)

	// Running the script throws a division by zero exception
	Console.WriteLine("Trying to run a script with runtime errors ...")
	Console.WriteLine()
	TRY
		? XSharpScript.EvaluateAsync(source):Result
	CATCH e AS AggregateException
		Console.WriteLine("Exception message: {0}",e:InnerException:Message)
	CATCH e AS Exception
		Console.WriteLine("Exception message: {0}",e:Message)
	END TRY

	RETURN


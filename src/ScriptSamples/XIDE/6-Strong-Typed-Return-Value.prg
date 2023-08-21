USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting

FUNCTION Start() AS VOID
	// A script that returns a float
	VAR lines := <STRING>{;
		"VAR X := 1234.5s",;
		"VAR Y := 5.4321s",;
		"return X/Y";
		}

	// Join with newlines to get the script source	
	VAR source := String.Join(e"\r\n", lines)

	// Print the source
	Console.WriteLine("The follwoing script returns a float:")
	Console.ForegroundColor := ConsoleColor.DarkGray
	Console.WriteLine(e"\n{0}\n",source)
	Console.ForegroundColor := ConsoleColor.Gray;

	// Run the script with a REAL8 return type
	BEGIN SCOPE
		Console.WriteLine(e"\nRun with REAL8 return type:")
		VAR result := XSharpScript.EvaluateAsync<REAL8>(source):Result
		Console.WriteLine("Result: {0} ({1})", result, result:GetType())
	END SCOPE

	// Run the script with a REAL4 return type
	BEGIN SCOPE
		Console.WriteLine(e"\nRun with REAL4 return type:")
		VAR result := XSharpScript.EvaluateAsync<REAL4>(source):Result
		Console.WriteLine("Result: {0} ({1})", result, result:GetType())
	END SCOPE

	RETURN


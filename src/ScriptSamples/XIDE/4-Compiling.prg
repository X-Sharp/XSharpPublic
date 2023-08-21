USING System.Linq
USING LanguageService.CodeAnalysis
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

	// It is necessary to create a script object to compile the script
	Console.WriteLine("Creating a script object from a source with errors ...")
	VAR script := XSharpScript.Create(source)

	// Compiling a script returns a list of diagnostic messages as ImmutableArray<Diagnostic>
	Console.WriteLine("Compiling the script ...")
	VAR diagnostics := script:Compile()

	Console.WriteLine()

	// Examine the diagnostics to check for errors. Multiple diagnostics may exist
	IF diagnostics:Select({|d| d:Severity = DiagnosticSeverity.Error}):Count() > 0
		Console.WriteLine("Compilation has errors:")
	ELSEIF diagnostics:Select({|d| d:Severity = DiagnosticSeverity.Warning}):Count() > 0
		Console.WriteLine("Compilation has warnings:")
	ELSE
		Console.WriteLine("Compilation successful!")
	ENDIF

	// Print all diagnostic messages
	FOREACH VAR d IN diagnostics
		Console.WriteLine(d)
	NEXT

	RETURN


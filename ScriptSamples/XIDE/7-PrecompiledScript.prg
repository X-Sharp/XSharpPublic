USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting

CLASS Globals
	PUBLIC N AS INT
END CLASS

FUNCTION Start() AS VOID
	// A script that returns a float
	VAR lines := <STRING>{;
		"N := N+1",;
		"Console.WriteLine('Hello, this is run #{0}',N)";
		}

	// Join with newlines to get the script source	
	VAR source := String.Join(e"\r\n", lines)

	// Print the source
	Console.WriteLine("The follwoing is the script source:")
	Console.ForegroundColor := ConsoleColor.DarkGray
	Console.WriteLine(e"\n{0}\n",source)
	Console.ForegroundColor := ConsoleColor.Gray;

	Console.Write(e"\nCompiling the script ... ")

	// Create the script
	VAR script := XSharpScript.Create(source, globalsType := typeof(Globals))
    
	// If not called explicitly, the script will be compiled during the first call to Run()
	script:Compile()

	Console.WriteLine("OK")

	VAR g := Globals{}

	// The script is ran with the initialized globals object
	Console.WriteLine(e"\nRunning the script:")
	script:RunAsync(g)

	// If the script is ran again with the same globals object, the globals state will be maintained
	Console.WriteLine(e"\nRunning the script again:")
	script:RunAsync(g)

	RETURN


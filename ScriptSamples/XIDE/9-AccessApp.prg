USING System.Reflection
USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting

FUNCTION Work() AS VOID
	Console.WriteLine("Working...")
	RETURN

FUNCTION Start() AS VOID
	// A script that attempts to access a function declared in this application
	// This does not work unless the script can reference the current assembly!
	VAR source := "Work()"

	// Print the source
	Console.WriteLine("The is the script source that needs to access a function declared in this app:")
	Console.ForegroundColor := ConsoleColor.DarkGray
	Console.WriteLine(e"\n{0}\n",source)
	Console.ForegroundColor := ConsoleColor.Gray;

	// To reference the current assembly we only need the assembly object, 
	// returned by Assembly.GetExecutingAssembly()
	Console.WriteLine(e"Running the script:\n")
	XSharpScript.RunAsync(source, ScriptOptions.Default.WithReferences(<Assembly>{ Assembly.GetExecutingAssembly() }))

	RETURN


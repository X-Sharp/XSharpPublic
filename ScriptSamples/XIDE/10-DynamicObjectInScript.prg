USING System.Reflection
USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting
USING Microsoft.CSharp

FUNCTION Start() AS VOID
	// To use the dynamic type we need to reference Microsoft.CSharp.dll
	// In this case we call the string:Split() method, late-bound in dynamic variable 'o'
	VAR source := "local o as dynamic; o := '123 456'; ? o:Split(' ')[1]"

	// Print the source
	Console.WriteLine("The is a script that uses dynamic:")
	Console.ForegroundColor := ConsoleColor.DarkGray
	Console.WriteLine(e"\n{0}\n",source)
	Console.ForegroundColor := ConsoleColor.Gray;

	// To reference the Microsoft.CSharp.dll we can reference it in the current 
	// assembly and pass the refrence to the script in-memory.
	Console.WriteLine(e"Run the script:\n")
	XSharpScript.RunAsync(source, ScriptOptions.Default.WithReferences(typeof(Microsoft.CSharp.RuntimeBinder.Binder):Assembly))

	RETURN


USING LanguageService.CodeAnalysis.Scripting
USING LanguageService.CodeAnalysis.XSharp.Scripting

FUNCTION Start() AS VOID
	// A script that looks a lot like a macro!
	VAR source := "{|o|o:GetType()}"

	// Print the source
	Console.WriteLine("The is the script source:")
	Console.ForegroundColor := ConsoleColor.DarkGray
	Console.WriteLine(e"\n{0}\n",source)
	Console.ForegroundColor := ConsoleColor.Gray;

	// Running the script returns a delegate to a lamda function!
	Console.WriteLine(e"\nThe script is evaluated to the delegate f of type: System.Func<OBJECT,System.Type>\n")
	VAR f := XSharpScript.EvaluateAsync<System.Func<OBJECT,System.Type>>(source):Result

	// The returned delegate can be used as any delegate
	Console.WriteLine("f(1): {0}", f(1))
	Console.WriteLine("f('1'): {0}", f('1'))
	Console.WriteLine(e"f(\"1\"): {0}", f("1"))

	RETURN


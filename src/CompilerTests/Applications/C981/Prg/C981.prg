// 981. Problems with the /fox3 option -  macrocompiler #2057
// https://github.com/X-Sharp/XSharpPublic/issues/2056

// Both with fox3+ and fox3-
//#pragma options("fox3", enable)

BEGIN NAMESPACE TestNS

CLASS TestFox
	STATIC EXPORT StaticExport AS INT
	STATIC PROPERTY StaticProperty AS INT AUTO
END CLASS

END NAMESPACE

CLASS NoNamespace
	STATIC EXPORT StaticExport AS INT
	STATIC PROPERTY StaticProperty AS INT AUTO
END CLASS

FUNCTION Start() AS VOID
// XSharp.MacroCompiler.CompilationError
// Macrocompiler (1,59): error XM0222: 'TestNS.TestFox' is not a valid expression term
? &("System.Int32.MaxValue")
? &("TestNS.TestFox.StaticExport")
? &("TestNS.TestFox.StaticProperty")
    
// those work fine:
? &("Int32.MaxValue")
? &("NoNamespace.StaticExport")
? &("NoNamespace.StaticProperty")


// 981. Problems with the /fox3 option -  macrocompiler #2057
// https://github.com/X-Sharp/XSharpPublic/issues/2057

// Both with fox3+ and fox3-
//#pragma options("fox3", enable)

GLOBAL TestGlobalFloat := 1.5 AS FLOAT
GLOBAL TestGlobalString := "test" AS STRING

BEGIN NAMESPACE TestNS

CLASS TestFox
	STATIC EXPORT StaticExport AS INT
	STATIC EXPORT StaticFloat AS FLOAT
	STATIC PROPERTY StaticProperty AS INT AUTO
END CLASS

END NAMESPACE

CLASS NoNamespace
	STATIC EXPORT StaticExport AS INT
	STATIC PROPERTY StaticProperty AS INT AUTO
	STATIC EXPORT TestFloat := 1.0 AS float
END CLASS

FUNCTION Start() AS VOID
// XSharp.MacroCompiler.CompilationError
// Macrocompiler (1,59): error XM0222: 'TestNS.TestFox' is not a valid expression term
? &("System.Int32.MaxValue")
? &("System.Int32.MaxValue == 2147483647")
? &("TestNS.TestFox.StaticExport")
? &("TestNS.TestFox.StaticProperty")
    
// those work fine:
? &("Int32.MaxValue")
? &("NoNamespace.StaticExport")
? &("NoNamespace.StaticProperty")
? &("NoNamespace.TestFloat == 1.0")

? &("TestGlobalFloat := 2.5")
? &("TestGlobalFloat")

? &("TestGlobalString := 'test.me'")
? &("TestGlobalString")

PROC xAssert(l AS LOGIC) AS VOID
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"


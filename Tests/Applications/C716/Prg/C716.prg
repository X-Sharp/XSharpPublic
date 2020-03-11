// 716. Macrocompiler problem with &cMacroToCompile syntax
// Following compiles and rns fine in VO, returning "3" both times. In X# it throws an exception
FUNCTION Start() AS VOID
	LOCAL c AS STRING
	c := "1 + 2"
	? c
	? &(c) // OK
	? &c   // Unhandled Exception: XSharp.Error: Variable does not exist
RETURN

//628. Runtime InvalidCastException with PSZ(_CAST, somedefine ) with the X# runtime

// works ok with literal value, but not with literal value in a DEFINE

DEFINE mydefine := "testDefine"

FUNCTION Start() AS VOID
	LOCAL p AS PSZ
	p := TestLiteral() // ok
	? p
	p := TestDefine() // runtime error
	? p
RETURN

FUNCTION TestLiteral() AS PSZ PASCAL
	LOCAL p AS PSZ
	p := PSZ(_CAST,"testLiteral")
	? p

	xAssertEquals(Psz2String(p) , "testLiteral")
RETURN p

FUNCTION TestDefine() AS PSZ PASCAL
	LOCAL p AS PSZ
	// Unhandled Exception: System.InvalidCastException: Specified cast is not valid.
	p := PSZ(_CAST,mydefine)
	? p

	xAssertEquals(Psz2String(p) , "testDefine")
RETURN p

/*
TestLiteral() is being emitted as:

.method public hidebysig static 
	void TestLiteral () cil managed 
{
	// Method begins at RVA 0x2118
	// Code size 51 (0x33)
	.maxstack 3
	.locals init (
		[0] class [mscorlib]System.Collections.Generic.List`1<native int>,
		[1] valuetype [XSharp.VO]XSharp.__Psz
	)

	IL_0000: nop
	IL_0001: newobj instance void class [mscorlib]System.Collections.Generic.List`1<native int>::.ctor()
	IL_0006: stloc.0
	.try
	{
		IL_0007: nop
		IL_0008: nop
		IL_0009: ldloca.s 1
		IL_000b: ldstr "test"
		IL_0010: ldloc.0
		IL_0011: call native int [XSharp.VO]XSharp.Internal.CompilerServices::String2Psz(string, class [mscorlib]System.Collections.Generic.List`1<native int>)
		IL_0016: call instance void [XSharp.VO]XSharp.__Psz::.ctor(native int)
		IL_001b: ldloc.1
		IL_001c: call valuetype [XSharp.VO]XSharp.__Usual [XSharp.VO]XSharp.__Usual::op_Implicit(valuetype [XSharp.VO]XSharp.__Psz)
		IL_0021: call void [XSharp.VO]XSharp.VO.Functions::QOut(valuetype [XSharp.VO]XSharp.__Usual)
		IL_0026: nop
		IL_0027: leave.s IL_0032
	} // end .try
	finally
	{
		IL_0029: nop
		IL_002a: ldloc.0
		IL_002b: call void [XSharp.VO]XSharp.Internal.CompilerServices::String2PszRelease(class [mscorlib]System.Collections.Generic.List`1<native int>)
		IL_0030: nop
		IL_0031: endfinally
	} // end handler

	IL_0032: ret
} // end of method Functions::TestLiteral



while TestDefine() is being emitted as:



.method public hidebysig static 
	void TestDefine () cil managed 
{
	// Method begins at RVA 0x20f0
	// Code size 28 (0x1c)
	.maxstack 1
	.locals init (
		[0] valuetype [XSharp.VO]XSharp.__Psz
	)

	IL_0000: nop
	IL_0001: nop
	IL_0002: ldstr "test"
	IL_0007: unbox.any [XSharp.VO]XSharp.__Psz
	IL_000c: stloc.0
	IL_000d: ldloc.0
	IL_000e: call valuetype [XSharp.VO]XSharp.__Usual [XSharp.VO]XSharp.__Usual::op_Implicit(valuetype [XSharp.VO]XSharp.__Psz)
	IL_0013: call void [XSharp.VO]XSharp.VO.Functions::QOut(valuetype [XSharp.VO]XSharp.__Usual)
	IL_0018: nop
	IL_0019: br.s IL_001b

	IL_001b: ret
} // end of method Functions::TestDefine

*/


PROC xAssertEquals(o1 AS STRING, o2 AS STRING)
IF o1 == o2
	? "Assertion passed"
ELSE
	THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" , o2 , o1)}
END IF

// 809. Program hangs with incorrect late bound call
// https://github.com/X-Sharp/XSharpPublic/issues/810    
// https://github.com/X-Sharp/XSharpPublic/issues/836
// /vo7+ and Target:x86
FUNCTION Start() AS VOID STRICT
TestClass{}:DoSomething()

FUNCTION ReturnFalse() AS LOGIC
RETURN FALSE

CLASS TestClass
	METHOD BadMethod(uParam AS USUAL ) AS VOID
	METHOD DoSomething() AS VOID
		IF ReturnFalse()
			LOCAL u := 1 AS USUAL
			SELF:BadMethod(@u)
		ENDIF
	RETURN // freezes here, but does not happen when you remove the RETURN statement!
END CLASS

/*
The IL generated for the DoSomething() method is:

.method public hidebysig newslot virtual 
	instance void DoSomething () cil managed 
{
	// Method begins at RVA 0x20f8
	// Code size 31 (0x1f)
	.maxstack 2
	.locals init (
		[0] bool,
		[1] valuetype [XSharp.RT]XSharp.__Usual
	)

	IL_0000: nop
	IL_0001: call bool xRuntime.Exe.Functions::ReturnFalse()
	IL_0006: stloc.0
	IL_0007: ldloc.0
	IL_0008: brfalse.s IL_001c

	IL_000a: nop
	IL_000b: ldc.i4.1
	IL_000c: call valuetype [XSharp.RT]XSharp.__Usual [XSharp.RT]XSharp.__Usual::op_Implicit(int32)
	IL_0011: stloc.1
	IL_0012: ldarg.0
	IL_0013: ldloca.s 1
	IL_0015: conv.u
	IL_0016: callvirt instance void TestClass::BadMethod(valuetype [XSharp.RT]XSharp.__Usual)
	IL_001b: nop

	IL_001c: br.s IL_001e  // <-----this is removed when we remove the RETURN statement

	IL_001e: ret
*/

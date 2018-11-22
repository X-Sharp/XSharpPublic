// 641. Runtime error with SHORT( _CAST , usual)
/*
   
The following "casts" (some of them taken form actual users' code) work correctly, except for the ShortInt casts, which throw an exception:

Unhandled Exception: XSharp.Error: Numeric overflow when converting USUAL of type 'LONGINT' to 'SHORT'
   at XSharp.__Usual.op_Implicit(__Usual u) 

Problem is the generated code by the compiler for the SHORTINT casts:

	// object i = (short)(u & (__Usual)65535);
	IL_000c: ldloc.0
	IL_000d: ldc.i4 65535
	IL_0012: call valuetype [XSharp.VO]XSharp.__Usual [XSharp.VO]XSharp.__Usual::op_Implicit(int32)
	IL_0017: call valuetype [XSharp.VO]XSharp.__Usual [XSharp.VO]XSharp.__Usual::op_BitwiseAnd(valuetype [XSharp.VO]XSharp.__Usual, valuetype [XSharp.VO]XSharp.__Usual)
	IL_001c: call int16 [XSharp.VO]XSharp.__Usual::op_Implicit(valuetype [XSharp.VO]XSharp.__Usual)
	IL_0021: box [mscorlib]System.Int16

this leaves a number between 32767 and 65535 in the stack after the BitwiseAnd for the following samples
and then the implicit operator to ShortInt throws an overflow because of the code:

CASE __UsualType.Long	; RETURN CHECKED((SHORT) u:_intValue)

We could make this UNCHECKED which would solve the problem, but this would make all implicit conversions 
unchecked, and this is not what we want, because we want regular conversions like SHORT(usual) to do 
overflow checking (unlike the "cast" opearations)

Maybe in the above case the compiler should insert code to call a special unchecked conversion?

Note that very similar code is generated also for WORD(_CAST,u) and BYTE(_CAST,u), but in those cases 
the USUAL conversion to BYTE/WORD is always safe after the bitwise And, since those are unsigned types 
so there's no overflow in those cases

*/
FUNCTION Start( ) AS VOID
	LOCAL u AS USUAL

	u := -100
	xAssert(BYTE(_CAST,u) == 156)
	xAssert(WORD(_CAST,u) == 65436)
	xAssert(INT(_CAST,u) == -100)
	xAssert(DWORD(_CAST,u) == 4294967196)
	xAssert(INT64(_CAST,u) == -100)
	xAssert(SHORT(_CAST,u) == -100) // error

	u := -40000
	xAssert(BYTE(_CAST,u) == 192)
	xAssert(WORD(_CAST,u) == 25536)
	xAssert(INT(_CAST,u) == -40000)
	xAssert(DWORD(_CAST,u) == 4294927296)
	xAssert(INT64(_CAST,u) == -40000)
	xAssert(SHORT(_CAST,u) == 25536) // error

	u := 40000
	xAssert(BYTE(_CAST,u) == 64)
	xAssert(WORD(_CAST,u) == 40000)
	xAssert(INT(_CAST,u) == 40000)
	xAssert(DWORD(_CAST,u) == 40000)
	xAssert(INT64(_CAST,u) == 40000)
	xAssert(SHORT(_CAST,u) == -25536) // error

	u := 100000
	xAssert(BYTE(_CAST,u) == 160)
	xAssert(WORD(_CAST,u) == 34464)
	xAssert(INT(_CAST,u) == 100000)
	xAssert(DWORD(_CAST,u) == 100000)
	xAssert(INT64(_CAST,u) == 100000)
	xAssert(SHORT(_CAST,u) == -31072) // error

RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"


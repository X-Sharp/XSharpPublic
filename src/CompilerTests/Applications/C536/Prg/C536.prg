// 536. Runtime stackoverflow adding USUAL + FLOAT
FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	LOCAL f AS FLOAT
	u := 10.0
	f := 100.0
	Test1(u,f)
	Test2(u,f)
	Test3(u,f)
	u := f + u
	? u
	IF u != 110.0
		THROW Exception{"Icorrect result"}
	END IF
RETURN

/*
Error happens inside the __VOFloat type in VulcanRTFuncs:

METHOD Add( n AS USUAL ) AS __VOFloat
IF n:IsFloat
   ret := Add( n )
   ...
which calls itself, hence the StackOverflow.

Looks like this method is never called in vulcan, though, so no problem when compilng with the vulcan compiler.
The difference is that vulcan converts the FLOAT value to USUAL and then uses the add opearator of the USUAL type:

vulcan:
IL_0002: ldarg.0
IL_0003: ldarg.1
IL_0004: call valuetype [VulcanRTFuncs]Vulcan.__Usual [VulcanRTFuncs]Vulcan.__Usual::op_Implicit(valuetype [VulcanRTFuncs]Vulcan.__VOFloat)
IL_0009: call valuetype [VulcanRTFuncs]Vulcan.__Usual [VulcanRTFuncs]Vulcan.__Usual::op_Addition(valuetype [VulcanRTFuncs]Vulcan.__Usual, valuetype [VulcanRTFuncs]Vulcan.__Usual)
IL_000e: nop
IL_000f: starg.s u

so the FLOAT:Add(usual) method of the FLOAT class is not used

But in x#, the 2 values are added using the add operator of the FLOAT type, which in turn
calls FLOAT:Add(usual) which causes the StackOverflow:

x#:
IL_0002: ldarg.0
IL_0003: ldarg.1
IL_0004: call valuetype [VulcanRTFuncs]Vulcan.__VOFloat [VulcanRTFuncs]Vulcan.__VOFloat::op_Addition(valuetype [VulcanRTFuncs]Vulcan.__Usual, valuetype [VulcanRTFuncs]Vulcan.__VOFloat)
IL_0009: call valuetype [VulcanRTFuncs]Vulcan.__Usual [VulcanRTFuncs]Vulcan.__Usual::op_Implicit(valuetype [VulcanRTFuncs]Vulcan.__VOFloat)
IL_000e: starg.s u
*/
PROCEDURE Test1(u AS USUAL , f AS FLOAT)
	u := u + f
RETURN 

PROCEDURE Test2(u AS USUAL , f AS FLOAT)
	u := f + u
RETURN 
PROCEDURE Test3(u AS USUAL , f AS FLOAT)
	f := f + u
RETURN 


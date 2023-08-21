// 542. StackOverflowException with FLOAT += USUAL
// Reported by Meinhard
/*
Problem is due to the Add(USUAL):FLOAT method of the FLOAT class in the vulcan runtime,
this accidentally call itself when the param is a FLOAT.

In vulcan this works, because the operation is executed on the usual type, not on float

for the same reason, in x# the operation FLOAT += (INT in) has no influnce on the result,
because in the same function in the runtime there's a bug also when the USUAL holds an INT
(in this case the code changes the value of the passed param, not in the returned value as it should..)

vulcan IL:
[0] valuetype [VulcanRTFuncs]Vulcan.__Usual u,
[1] valuetype [VulcanRTFuncs]Vulcan.__VOFloat f
...
ldloc.1
call valuetype [VulcanRTFuncs]Vulcan.__Usual [VulcanRTFuncs]Vulcan.__Usual::op_Implicit(valuetype [VulcanRTFuncs]Vulcan.__VOFloat)
ldloc.0
call valuetype [VulcanRTFuncs]Vulcan.__Usual [VulcanRTFuncs]Vulcan.__Usual::op_Addition(valuetype [VulcanRTFuncs]Vulcan.__Usual, valuetype [VulcanRTFuncs]Vulcan.__Usual)
call valuetype [VulcanRTFuncs]Vulcan.__VOFloat [VulcanRTFuncs]Vulcan.__Usual::op_Implicit(valuetype [VulcanRTFuncs]Vulcan.__Usual)
stloc.1


x# IL:
[0] valuetype [VulcanRTFuncs]Vulcan.__Usual u,
[1] valuetype [VulcanRTFuncs]Vulcan.__VOFloat f
...
ldloc.1
ldloc.0
call valuetype [VulcanRTFuncs]Vulcan.__VOFloat [VulcanRTFuncs]Vulcan.__VOFloat::op_Addition(valuetype [VulcanRTFuncs]Vulcan.__VOFloat, valuetype [VulcanRTFuncs]Vulcan.__Usual)
stloc.1
*/
#pragma warnings(162, off) // unreachanle
PROCEDURE DoTest()
	LOCAL u := 1.0 AS USUAL
//	LOCAL u := 1 AS USUAL
	LOCAL f := 1.0 AS FLOAT
	f += u
RETURN

FUNCTION Start( ) AS VOID
	DoTest()
RETURN
	LOCAL u AS USUAL
	LOCAL f AS FLOAT

	u := 1.0
	f := 2.0
	f += u
	? f

	IF f != 3.0
		THROW Exception{"Incorrect result adding FLOAT in USUAL + FLOAT"}
	END IF


	u := 1
	f := 2.0
	f += u

	IF f != 3.0
		THROW Exception{"Incorrect result adding INT in USUAL + FLOAT"}
	END IF
RETURN


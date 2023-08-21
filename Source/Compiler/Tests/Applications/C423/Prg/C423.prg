// 423. Runtime error comparing NIL to SYMBOL
/*
vulcan code:

ldstr "TEST"
newobj instance void [VulcanRTFuncs]Vulcan.__Symbol::.ctor(string)
call valuetype [VulcanRTFuncs]Vulcan.__Usual [VulcanRTFuncs]Vulcan.__Usual::op_Implicit(valuetype [VulcanRTFuncs]Vulcan.__Symbol)
call bool [VulcanRTFuncs]Vulcan.__Usual::op_Equality(valuetype [VulcanRTFuncs]Vulcan.__Usual, valuetype [VulcanRTFuncs]Vulcan.__Usual)

x# code:

ldstr "TEST"
newobj instance void [VulcanRTFuncs]Vulcan.__Symbol::.ctor(string)
call bool [VulcanRTFuncs]Vulcan.__Symbol::op_Equality(valuetype [VulcanRTFuncs]Vulcan.__Usual, valuetype [VulcanRTFuncs]Vulcan.__Symbol)

*/
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL l AS LOGIC

? NIL == #TEST
? #TEST == NIL
xAssert(NIL != #TEST)
xAssert(#TEST != NIL)
xAssert(.not.(NIL == #TEST))
xAssert(.not.(#TEST == NIL))

u := NIL
l := u == #TEST // exception
? l

? u == #TEST
? #TEST == u
? u == NIL
? NIL == u

xAssert(u != #TEST)
xAssert(#TEST != u)
xAssert(.not. (u == #TEST))
xAssert(.not. (#TEST) == u)
xAssert(u == NIL)


// all the rest is the original report. not needed anymore after finding the root 
// cause of the problem , but I left it, just in case
LOCAL cb AS CODEBLOCK
cb := {|s| s == #TEST}
? Eval(cb , #TEST)
? Eval(cb , NIL)

? AScan({NIL,NIL} , {|element| element == #TEST})

LOCAL aTest AS ARRAY
LOCAL nScan AS DWORD
LOCAL sName AS SYMBOL

sName := #AAA
aTest := {sName,NIL}

nScan := AScan(aTest , {|element| element == NIL})
xAssert(nScan == 2)

nScan := AScan(aTest , {|element| NIL == element})
xAssert(nScan == 2)

nScan := AScan(aTest , {|element| element == sName})
xAssert(nScan == 1)
nScan := AScan(aTest , {|element| sName == element})
xAssert(nScan == 1)

// exception below when comparing to NIL
sName := #BBB
nScan := AScan(aTest , {|element| element == sName})
xAssert(nScan == 0)
nScan := AScan(aTest , {|element| sName == element})
xAssert(nScan == 0)

RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF


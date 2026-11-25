// 993. Problems with the /fox3 option - usual properties in codeblocks

USING System.Collections

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

CLASS TestCodeblockProperties
	PROPERTY TypedProp AS ArrayList AUTO
	PROPERTY UnTypedProp AS USUAL AUTO

	PROPERTY myself AS USUAL GET SELF

	CONSTRUCTOR()
		TypedProp := ArrayList{}
		TypedProp:Add(123)
		UnTypedProp := TypedProp

		LOCAL cb AS CODEBLOCK

		? TypedProp.Count // 1, OK
		cb := {|| TypedProp.Count}
		Eval(cb) // 1, OK

		? UnTypedProp.Count // 1, OK
		cb := {|| UnTypedProp.Count}
		Eval(cb) // Variable does not exist: MYSELF
		
		? myself.UnTypedProp.Count // 1, OK
		cb := {|| myself.TypedProp.Count}
		Eval(cb) // Variable does not exist: MYSELF
		
END CLASS

FUNCTION Start() AS VOID
	TestCodeblockProperties{}



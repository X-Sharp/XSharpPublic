// https://github.com/X-Sharp/XSharpPublic/issues/785
// Virtual Members of Sealed classes
// https://github.com/X-Sharp/XSharpPublic/issues/846
// Virtual and Override
// https://github.com/X-Sharp/XSharpPublic/issues/786
// Make OVERRIDE mandatory

#pragma options("vo3", on)
#pragma options("enforceoverride", on)
#pragma warnings(67, off) // event not used
#pragma warnings(114, off) // hide member in parent class
FUNCTION Start() AS VOID
LOCAL o AS Parent
o := Child{}
o:Test("A")
RETURN

CLASS Parent
	VIRTUAL METHOD Test(a AS STRING) AS VOID
		? "parent"
	VIRTUAL EVENT OnTest AS EventHandler
END CLASS

CLASS Child INHERIT Parent
	OVERRIDE METHOD Test(a AS STRING) AS VOID
		? "child"

	METHOD Test2(a AS STRING) AS VOID
		? "child"
	NEW EVENT OnTest AS EventHandler
END CLASS

PARTIAL SEALED CLASS A
METHOD Dummy(xx AS OBJECT) AS STRING
RETURN ""
EVENT OnDummy AS EVentHandler
END CLASS


METHOD Dummy2 AS LOGIC CLASS A
    RETURN  TRUE


#pragma options("vo3", on)
#pragma options("enforceoverride", on)
CLASS parent1
	METHOD Test() AS VOID
END CLASS
CLASS child1 INHERIT parent1
	METHOD Test() AS VOID
END CLASS



#pragma options("vo3", off)
#pragma options("enforceoverride", off)

// without enforceoverride the keyword VIRTUAL for the child is interpreted as OVERRIDE
// but that keyword is not stricly needed

CLASS parent2
    PUBLIC Foo AS STRING
	VIRTUAL METHOD Test() AS VOID
END CLASS
CLASS child2 INHERIT parent2
	METHOD Test() AS VOID
END CLASS



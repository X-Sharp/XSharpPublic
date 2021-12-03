// https://github.com/X-Sharp/XSharpPublic/issues/785
// Virtual Members of Sealed classes
// https://github.com/X-Sharp/XSharpPublic/issues/846
// Virtual and Override
// https://github.com/X-Sharp/XSharpPublic/issues/786
// Make OVERRIDE mandatory

#pragma options("vo3", on)
#pragma options("enforceoverride", on)
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
	METHOD Skata() AS VOID
END CLASS
CLASS child1 INHERIT parent1
	METHOD Skata() AS VOID
END CLASS

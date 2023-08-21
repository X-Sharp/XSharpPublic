// 114. /vo3 option does not make properties defined ass access virtual
// warning XS0108: 'Child.acc' hides inherited member 'Parent.acc'. Use the new keyword if hiding was intended.

// compile with /vo3
CLASS Parent
	ACCESS acc AS STRING
	RETURN "Parent"
	METHOD met() AS STRING
	RETURN "Parent"
	
END CLASS
CLASS Child INHERIT Parent
	ACCESS acc AS STRING
	RETURN "Child"
	METHOD met() AS STRING
	RETURN "Child"
END CLASS
FUNCTION Start() AS VOID
	LOCAL o AS Parent
	o := Child{}
	? o:met() // ok
	? o:acc   // calls parent
	
	IF o:met() != "Child"
		THROW Exception{"Parent incorrectly called"}
	END IF
	IF o:acc != "Child"
		THROW Exception{"Parent incorrectly called"}
	END IF


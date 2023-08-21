// 176. Incorrect value passed to super clipper constructor
FUNCTION Start( ) AS VOID
	Child{123}
RETURN

CLASS Parent
CONSTRUCTOR(u)
	IF u != 123
		THROW Exception{"Incorrect value passed to parent constructor"}
	END IF
END CLASS

CLASS Child INHERIT Parent
CONSTRUCTOR(u)
SUPER(u)
END CLASS


// 472. error XS1729: 'object' does not contain a constructor that takes 1 arguments

FUNCTION Start( ) AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o
	
	LOCAL c AS Clipboard
	c := Clipboard{}
	? c
RETURN

CLASS TestClass
END CLASS

CLASS Clipboard INHERIT VObject
END CLASS

CLASS Parent
END CLASS
CLASS Child INHERIT Parent
END CLASS

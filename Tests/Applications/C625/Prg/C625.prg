// 625. Namespace aliases not working
// error XS0246: The type or namespace name 'abc' could not be found (are you missing a using directive or an assembly reference?)

USING abc := a.b.c

BEGIN NAMESPACE a.b.c
	CLASS d
	END CLASS
END NAMESPACE

FUNCTION Start( ) AS VOID
	LOCAL oo AS a.b.c.d
	oo := a.b.c.d{} // ok
	LOCAL o AS abc.d
	o := abc.d{} // error
	? o:ToString()
RETURN

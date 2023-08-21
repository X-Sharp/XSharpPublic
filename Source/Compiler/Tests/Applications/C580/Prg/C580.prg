// 580. warning XS9015: No explicit constructor chaining; generating implicit call to super constructor.
/*
When compiling this code in VO/Vulcan dialect, a warning is reported:

warning XS9015: No explicit constructor chaining; generating implicit call to super constructor.

When compiling in Core, such a warning is not emitted. Most probably the crrect behavior
would be to make this warning be reported in all dialects, although also completely 
removing this from all dialects doesn't sound bad to me either.

In any case, I am just logging this in order to look into it some time in the future only.
I suggest not changing it now (11/2017) because not only it can break other things in the 
sensitive construtor() area, but also re-enabling the warning now may "break" existing (Core) code,
because the current templates for the form designer do not include a SUPER() call in the consttructor.
So there's plenty of code like that out there, and since most projects have "warnings as errors" enabled..
*/
FUNCTION Start( ) AS VOID
	? ChildClass{}
RETURN

CLASS ParentClass
END CLASS

CLASS ChildClass INHERIT ParentClass
	CONSTRUCTOR()
	RETURN
END CLASS

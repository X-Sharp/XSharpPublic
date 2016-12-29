// 201. error XS0118: 'System' IS a namespace but IS used like a type
// real problem is that if there is a reference with name 'Somename' or the 
// current assembly has that name, then it is not possible to use a class
// that has the same name ('Somename'). Could not enter it as such in the test suite,
// thus used the 'System' name as a class. Underlying problem is the same, though.

USING SomeNamespace

CLASS SomeNamespace.System
EXPORT n AS INT
END CLASS

FUNCTION Start() AS VOID
LOCAL o AS System
o := System{}
? o:n


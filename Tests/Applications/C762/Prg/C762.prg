// 762. Inconsistency with iif() and SByte in PROPERTY GETter
/*
In previous builds, the following code resulted to one warning inside the FUNCTION and no warnings or errors in the PROPERTY

Now, the same warning gets reproted in the function as before, but there's now also an error (for otherwise identical code) in the PROPERTY
*/

FUNCTION Start() AS VOID
LOCAL sb := 0 AS SByte
sb := iif(FALSE,sb,0) // warning XS9020: Narrowing conversion from 'int' to 'sbyte' may lead to loss of data or overflow errors

CLASS Foo
	EXPORT sb AS SByte
	PROPERTY SBProp AS SByte GET iif(FALSE,sb,0) // error XS0029: Cannot implicitly convert type 'byte' to 'sbyte'
END CLASS

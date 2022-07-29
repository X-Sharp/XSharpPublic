FUNCTION Start() AS VOID
LOCAL n := 1 AS INT

? Left("asd",n) // warning XS9021: Conversion from 'int' to 'dword' may lead to loss of data or overflow errors
? "asd" + Left("asd",n) // no warning!
? n:ToString() + Left("asd",n) // warning XS9021
RETURN

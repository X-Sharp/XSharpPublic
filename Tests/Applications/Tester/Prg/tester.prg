FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
o:ToString()

CLASS TestClass
NEW METHOD ToString() AS STRING
RETURN "123"
END CLASS

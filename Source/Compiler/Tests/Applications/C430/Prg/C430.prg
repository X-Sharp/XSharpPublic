// 430. Compiler crash with ASTYPE (is this the correct syntax?)
FUNCTION Start() AS VOID
LOCAL o AS OBJECT
LOCAL t AS TestClass
o := TestClass{}
t := o ASTYPE TestClass 
? t
o := Error{}
t := o ASTYPE TestClass
? t
RETURN

CLASS TestClass
END CLASS

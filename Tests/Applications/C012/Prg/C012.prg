// 12. compiler crash
#pragma warnings(168, off)   // variable is declared but never used
CLASS GenericClass<t>
END CLASS
FUNCTION Start() AS VOID
LOCAL o AS GenericClass<INT>


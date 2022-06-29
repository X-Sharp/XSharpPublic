// 6. "USING" seems to make the class not to be emitted at all. With #using it works as expected
#pragma warnings(168, off)   // variable is declared but never used
USING System
CLASS TestClass
METHOD Test() AS VOID
LOCAL n AS Int32
END CLASS

FUNCTION Start() AS VOID
TEstClass{}
RETURN

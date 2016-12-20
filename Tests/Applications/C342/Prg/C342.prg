// 342. error XS9040: The type of the first ASSIGN parameter must match the returntype of the ACCESS.
// This is from the SDK. Note that both types are the same of course, only casing is different
CLASS TestClass
ACCESS MaxPosition AS POINT STRICT 
RETURN NULL
ASSIGN MaxPosition(oPoint AS point)  STRICT 


// from user code
ACCESS MyDate AS DateTime
RETURN DateTime.Now
ASSIGN MyDate(d AS System.DateTime)

END CLASS

CLASS Point
END CLASS

FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
o:MaxPosition := Point{}
? o:MaxPosition


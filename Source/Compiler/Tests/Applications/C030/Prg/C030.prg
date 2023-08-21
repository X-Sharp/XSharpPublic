// 30. Argument 1: cannot convert from ...
FUNCTION Start() AS VOID
? System.Drawing.Font{"Arial" , 14.0} // error
? System.Drawing.Font{"Arial" , (REAL4)14.0} // works ok


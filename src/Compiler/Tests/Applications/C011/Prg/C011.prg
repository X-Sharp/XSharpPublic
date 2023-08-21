// 11.  error XS1061: 'TestClass' does not contain a definition for 'p' and no extension method 'p' 
// and
// error XS0103: The name 'p' does not exist in the current context
CLASS TestClass
PROTECT p AS INT // PROTECTed is OK
METHOD Test() AS VOID
SELF:p := 1
p := 1
END CLASS

FUNCTION Start() AS VOID

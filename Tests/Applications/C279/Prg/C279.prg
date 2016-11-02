// error XS0266: Cannot implicitly convert type 'int' to 'byte'. An explicit conversion exists (are you missing a cast?)
// vulcan incompatibility (smallest numeric literal in  C# is Int32)
FUNCTION Start() AS VOID
LOCAL b AS BYTE
LOCAL l := TRUE AS LOGIC
b := iif ( l , 1 , 2 )
? b


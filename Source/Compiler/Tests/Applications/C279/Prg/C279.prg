// error XS0266: Cannot implicitly convert type 'int' to 'byte'. An explicit conversion exists (are you missing a cast?)
// vulcan incompatibility (smallest numeric literal in  C# is Int32)

// it compiles now in x# with /vo4+
// in vulcan it does not require /vo4+ to be set
// we must either allow it in x# without /vo4, too, or document it as a known difference with vulcan
FUNCTION Start() AS VOID
LOCAL b AS BYTE
LOCAL l := TRUE AS LOGIC
b := iif ( l , 1 , 2 )
? b


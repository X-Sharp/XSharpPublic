// https://github.com/X-Sharp/XSharpPublic/issues/1088
// Errors should be reported in the right columns
FUNCTION Start() AS VOID
LOCAL n := 1 AS INT
LOCAL c := "" AS STRING
LOCAL f := 1.0 AS FLOAT
SubStr( Left(c,n) , 1 )
 SubStr( Left(c,n) , 1 )
  SubStr( Left(c,f) , 1 )


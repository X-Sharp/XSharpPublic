// error XS0121: The call is ambiguous between the following methods or properties: 
// 'StringBuilder.Append(string)' and 'StringBuilder.Append(bool)'
// /vo10
FUNCTION Start() AS VOID
LOCAL c AS System.Text.StringBuilder
LOCAL l := TRUE AS LOGIC
c := System.Text.StringBuilder{}
c:Append( IIF( l, ' ', IIF( l, 'T', 'F') ) )


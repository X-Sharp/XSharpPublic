// 302. error XS0121: The call is ambiguous between the following methods or properties: 'Functions.Transform(__VODate, string)' and 'Functions.Transform(logic, string)'
FUNCTION Start() AS VOID
LOCAL u := 123 AS USUAL
LOCAL a := {1,2,3} AS USUAL
? Transform(u , "###.###")
? Len(a)


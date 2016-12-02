// 316.  error XS0457: Ambiguous user defined conversions '__Usual.implicit operator __Usual(object)' and '__Usual.implicit operator __Usual(sbyte)' when converting from 'char' to '__Usual'
// related to C231
// /vo4+
FUNCTION Start() AS VOID
LOCAL c := 'A' AS Char
LOCAL u AS USUAL
u := c
? u
? c



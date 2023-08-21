// 316.  error XS0457: Ambiguous user defined conversions '__Usual.implicit operator __Usual(object)' and '__Usual.implicit operator __Usual(sbyte)' when converting from 'char' to '__Usual'
// related to C231
// Vulcan allows conversion between char and word and byte 
// None of these require /vo
FUNCTION Start() AS VOID
LOCAL c := 'A' AS Char
LOCAL b AS BYTE
LOCAL w AS WORD
LOCAL u AS USUAL
u := c
? u
? c
b := 10          
w := 12
c := b 
c := w
? c


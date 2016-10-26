// 55. error XS0029: Cannot implicitly convert type 'string' to 'char'
//error XS0019: Operator '==' cannot be applied to operands of type 'char' and 'string'
FUNCTION Start() AS VOID
LOCAL c AS Char
c := 'a' // ok
? c == 'a' // ok
c := '"' // error
? c == '"' //error


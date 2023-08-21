// 295. error XS0034: Operator '&' is ambiguous on operands of type 'dword' and 'int'
// no matter of /vo4
FUNCTION Start() AS VOID
LOCAL dwNewStyles := 3 AS DWORD         
dwNewStyles :=  _And(dwNewStyles, _not(1)) 
? dwNewStyles

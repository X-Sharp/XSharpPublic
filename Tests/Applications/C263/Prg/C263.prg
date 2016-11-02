// 263. error XS0201: Only assignment, call, increment, decrement, and new object expressions can be used as a statement
FUNCTION Start() AS VOID
LOCAL liPosition, nItemNumber AS INT
nItemNumber := 1
(liPosition := nItemNumber)  
? liPosition


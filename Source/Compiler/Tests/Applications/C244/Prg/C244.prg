// 244. error XS0131: The left-hand side of an assignment must be a variable, property or indexer
FUNCTION Start() AS VOID
LOCAL bp AS BYTE PTR
LOCAL b AS BYTE
bp := @b
BYTE(bp) := 123
? b


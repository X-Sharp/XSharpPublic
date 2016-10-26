// 165. error XS0131: The left-hand side of an assignment must be a variable, property or indexer
FUNCTION Start() AS VOID
LOCAL p AS BYTE PTR
LOCAL b AS BYTE
p := @b
BYTE(p++) := 1
BYTE(++p) := 2


// 192. error XS0119: 'int' is a type, which is not valid in the given context
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := 1
DO CASE
CASE UsualType(u) == INT
CASE UsualType(u) == LOGIC
CASE UsualType(u) == FLOAT
END CASE


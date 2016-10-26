// 260. error XS0246: The type or namespace name 'TestFunc' could not be found
FUNCTION TestFunc(n AS INT) AS INT
RETURN 0
FUNCTION Start() AS VOID
LOCAL pFunc AS TestFunc PTR
pFunc := NULL_PTR


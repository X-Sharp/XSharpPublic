STATIC GLOBAL pfnSomeFunc AS SomeFunc PTR

FUNCTION SomeFunc(param1 AS INT, param2 AS INT , param3 AS INT) AS VOID STRICT
LOCAL n AS INT
PCALL(pfnSomeFunc, param1 , param2 , n)

FUNCTION Start() AS VOID


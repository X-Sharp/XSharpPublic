// 338. error XS0136: A local or parameter named 'param1' cannot be declared in this scope because that name is used in an enclosing local scope to define a local or parameter
#pragma warnings(165, off) // unassigned local
#pragma warnings(649, off) // global not used
STATIC GLOBAL pfnSomeFunc AS SomeFunc PTR

FUNCTION SomeFunc(param1 AS INT, param2 AS INT , param3 AS INT) AS VOID STRICT
LOCAL n AS INT
PCALL(pfnSomeFunc, param1 , param2 , n)

FUNCTION Start() AS VOID


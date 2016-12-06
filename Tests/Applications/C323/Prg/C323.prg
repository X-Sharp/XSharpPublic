// 323. error XS0029: Cannot implicitly convert type 'logic*' to 'object'
// error XS0030: Cannot convert type 'object' to 'void*'
FUNCTION Start() AS VOID
LOCAL lMemoHandle AS LOGIC
LOCAL oMemoHandle AS OBJECT
oMemoHandle := @lMemoHandle
lMemoHandle := (PTR)oMemoHandle != NULL_PTR
? lMemoHandle


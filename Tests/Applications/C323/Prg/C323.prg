// 323. error XS0029: Cannot implicitly convert type 'logic*' to 'object'
// error XS0030: Cannot convert type 'object' to 'void*'
// LOGIC * to OBJECT does not compile in Vulcan too
// But we made it possible in X# when /vo7 is enabled
FUNCTION Start() AS VOID
LOCAL lMemoHandle AS LOGIC
LOCAL oMemoHandle AS OBJECT
oMemoHandle := @lMemoHandle
lMemoHandle := oMemoHandle != NULL
? lMemoHandle


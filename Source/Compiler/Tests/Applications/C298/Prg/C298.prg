// 298. error XS0030: Cannot convert type 'logic' to 'dword'
// also used a lot in the SDK
FUNCTION Start() AS VOID
LOCAL l := TRUE AS LOGIC
? DWORD(_CAST , l)
? INT(_CAST , l)


// 336. error XS0037: Cannot convert null to 'IntPtr' because it is a non-nullable value type
FUNCTION Start() AS VOID
LOCAL tempHandle AS IntPtr
tempHandle := NULL
? tempHandle == NULL


// 251. error XS0103: The name 'PCallNative' does not exist in the current context
FUNCTION Start() AS VOID
LOCAL pFunc AS PTR
PCallNative<INT>( pFunc , 1)


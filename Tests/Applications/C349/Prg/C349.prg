// 349. error XS0221: Constant value '-1' cannot be converted to a 'dword' (use 'unchecked' syntax to override)
FUNCTION Start() AS VOID
Test( DWORD( _CAST, -1 ) ) // OK
Test( WORD( _CAST, -1 ) )  // error XS0221
Test( BYTE( _CAST, -1 ) )  // error XS0221

// Test( Uint64( _CAST, -1 ) )  // even vulcan complains about this :)

FUNCTION Test(d AS DWORD) AS VOID
? AsHexString(d)


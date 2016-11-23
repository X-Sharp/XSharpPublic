// 301. error XS0103: The name 'LBMessages' does not exist in the current context
// same as (fixed) C262, but with:
// /ns:Some.NS
GLOBAL DIM LBMessages [11,2] AS DWORD

FUNCTION Start() AS VOID
? LBMessages[1,1]


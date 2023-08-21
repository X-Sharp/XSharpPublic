// 485. Compiler crash with bad code in Core
FUNCTION Start() AS VOID
BadCodeInCore(1,,3)
BadCodeInCore(1,,3)
BadCodeInCore(,,,)
BadCodeInCore(,,,1)
BadCodeInCore(1,,,)
RETURN

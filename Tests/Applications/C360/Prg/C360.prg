// 360. (runtime) Unhandled Exception: System.InvalidOperationException: Nullable object must have a value.
// /vo2+, so p1 := NULL_STRING should default to a ""
FUNCTION DBFDebug( p1 := NULL_STRING AS STRING) AS VOID
? p1 == NULL // .T., should be false
? p1 == NULL_STRING // .F., should be true
? SLen(p1)
? p1:IndexOf('a')

FUNCTION Start() AS VOID
DBFDebug()


// 370. error XS0037: Cannot convert null to '__Psz' because it is a non-nullable value type
FUNCTION Start() AS VOID
LOCAL p := NULL AS PSZ
? p == NULL
? p != NULL

// those are ok
LOCAL p2 AS PTR
p2 := NULL
? p2 == NULL
? p2 != NULL


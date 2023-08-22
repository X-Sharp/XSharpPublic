// 583. error XS9002: Parser: missing CLASS at 'staticGlobal'

// Harbour has (also?) this syntax for STATIC globals:
STATIC staticGlobal := {}
FUNCTION Start() AS VOID
	? ALen(staticGlobal)
RETURN

// 587. error XS7038: Failed to emit module 'C587'.
// Should report an error message about unknown identifier "unknown"
// problem is related to the :Item property
FUNCTION Start() AS VOID
	? unknown:Item["aa"]
	? unknown:Item[123]
RETURN

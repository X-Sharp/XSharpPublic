// 546. error XS9052: In .Net you cannot take the address of a method or a function. For callback functions you need to use a delegate instead.

// That's code I've seen oftern in VO
// No point supporting this syntax I think, but the current error message is bogus I think
FUNCTION Start( ) AS VOID
	LOCAL pszDrives AS BYTE PTR
	pszDrives := PTR( BYTE, MemAlloc( 100 ) )
	// code fix:
	pszDrives := MemAlloc( 100 )
RETURN

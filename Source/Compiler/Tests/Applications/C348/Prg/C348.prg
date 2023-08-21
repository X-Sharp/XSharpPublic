// 348. error XS9042: Override of virtual method 'Append' has CLIPPER calling convention but overridden method in parent class is STRICT.
CLASS DBServerInherit INHERIT DBServer
VIRTUAL METHOD Append( lReleaseLocks ) 
RETURN FALSE
END CLASS

FUNCTION Start() AS VOID
	

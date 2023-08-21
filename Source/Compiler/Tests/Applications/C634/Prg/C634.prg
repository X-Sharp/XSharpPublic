// 634. error XS0127: Since 'TestClass.AsSystemVoid_STRICT()' returns void, a return keyword must not be followed by an object expression
// /vo9+
// Problem is only when return type is specified as System.Void. VOID works fine.
// note also the incorrect warnigs about Missing RETURN statements.
CLASS TestClass
	METHOD As_SystemVoid_STRICT() AS System.Void STRICT // error
	METHOD As_SystemVoid_CLIPPER() AS System.Void CLIPPER // error
	METHOD As_VOID_STRICT() AS VOID STRICT // ok
	METHOD As_VOID_CLIPPER() AS VOID CLIPPER // ok
END CLASS


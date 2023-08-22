// 540. (warning) error XS9043: Method 'UsualType' is ambiguous. Could be 'VulcanRTFuncs.Functions.UsualType(Vulcan.__Usual)' or 'Vulcan.UsualType'. Using the first one.

// Warning received when compiling the Vulcan SDK
// There's no other method UsualType(), only an 
// INTERNAL ENUM Vulcan.UsualType
// defined in VulcanRuntimeFuncs.dll

// This should not affect the UsualType() function
FUNCTION Start( ) AS VOID
	? UsualType(NIL) // ok
RETURN

BEGIN NAMESPACE Vulcan.VO
	CLASS Window
		METHOD Test() AS VOID
			? UsualType(NIL) // warning
		RETURN
	END CLASS
END NAMESPACE

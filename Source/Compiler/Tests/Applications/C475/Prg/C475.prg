// C475. Problems with comparing SYMBOL/DATE to NULL/NULL_OBJECT
FUNCTION Start( ) AS VOID
	LOCAL l AS LOGIC

	LOCAL s AS SYMBOL
	
	// runtime exception System.NullReferenceException:
/*
vulcan uses:
	IL_0016: ldloc.1
	IL_0017: ldnull
	IL_0018: call valuetype [VulcanRTFuncs]Vulcan.__Symbol [VulcanRTFuncs]Vulcan.__Symbol::op_Implicit(string)
	IL_001d: call bool [VulcanRTFuncs]Vulcan.__Symbol::op_Equality(valuetype [VulcanRTFuncs]Vulcan.__Symbol, valuetype [VulcanRTFuncs]Vulcan.__Symbol)
	IL_0022: stloc.0

while x#:
	IL_0013: ldloc.1
	IL_0014: ldnull
	IL_0015: call bool [VulcanRTFuncs]Vulcan.__Symbol::op_Equality(valuetype [VulcanRTFuncs]Vulcan.__Symbol, object)

In any case, I think it should be an error comparing SYMBOL (a structure) to NULL/NULL_OBJECT, but unfortunately vulcan does allow it..
*/
   s := NULL_SYMBOL                                                  
	l := s == NULL // vulcan returns true, x# crashes
	? l
	l :=  s == NULL_OBJECT // vulcan returns true, x# crashes
	? l

	s := #ABC
	? s == NULL // vulcan returns false, x# crashes
	? s == NULL_OBJECT // vulcan returns false, x# crashes
	


	// x# the following compile and always return FALSE
	// vulcan reports error VN4159: operator '==' is not defined for types 'DATE' and 'OBJECT'
	// I think x# should report an error, too.
	LOCAL d AS DATE
	?  d == NULL_OBJECT
	? d == NULL
	d := Today()
	? d == NULL_OBJECT
	? d == NULL
//

	// Both x# and vulcan correcty report an error on those:
	
	LOCAL sss AS TestStruct
	? sss == NULL
	? sss == NULL_OBJECT

RETURN

STRUCTURE TestStruct
	EXPORT n AS INT
END STRUCTURE

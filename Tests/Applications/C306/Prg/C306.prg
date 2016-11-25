// 306. error XS0261: Partial declarations of 'MiXeDcAsE' must be all classes, all structs, or all interfaces
FUNCTION Start() AS VOID
LOCAL o AS MixedCase
o := MixedCase{}
? o
	
PARTIAL CLASS MixedCase
END CLASS

PARTIAL CLASS MiXeDcAsE
END CLASS

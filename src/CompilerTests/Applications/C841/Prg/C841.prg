// 841. Failed to emit module error, with iif() - VO dialect
// /vo10+ enabled, shouldn't this allow the code to compile?
#pragma options("vo10", on)
FUNCTION Start() AS VOID
LOCAL u          
local cond 
cond := TRUE
u := IIF(.t., {}, NULL) // OK
u := IIF(.t., NULL, {}) // OK
u := IIF(.t., 456, NULL)  // error XS0173: Type of conditional expression cannot be determined because there is no implicit conversion between 'int' and '<null>'
u := IIF(.t., NULL , 456) // error XS0173
u := IIF(.t., 456, NULL_OBJECT)  // error XS0173
u := IIF(.t., NULL_OBJECT , 456) // error XS0173        
? u
cond := false
u := IIF(cond, {}, NULL) // OK
u := IIF(cond, NULL, {}) // OK
u := IIF(cond, 456, NULL)  // error XS0173: Type of conditional expression cannot be determined because there is no implicit conversion between 'int' and '<null>'
u := IIF(cond, NULL , 456) // error XS0173
u := IIF(cond, 456, NULL_OBJECT)  // error XS0173
u := IIF(cond, NULL_OBJECT , 456) // error XS0173        
? u
                                                                              

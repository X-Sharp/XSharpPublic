// 841. Failed to emit module error, with iif() - VO dialect
// /vo10+ enabled, shouldn't this allow the code to compile?
FUNCTION Start() AS VOID
LOCAL u
u := IIF(.t., {}, NULL) // OK
u := IIF(.t., NULL, {}) // OK
u := IIF(.t., 456, NULL)  // error XS0173: Type of conditional expression cannot be determined because there is no implicit conversion between 'int' and '<null>'
u := IIF(.t., NULL , 456) // error XS0173
u := IIF(.t., 456, NULL_OBJECT)  // error XS0173
u := IIF(.t., NULL_OBJECT , 456) // error XS0173


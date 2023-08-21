// 440. error XS0283: The type 'void*' cannot be declared const

// !!! IMPORTANT !!!
// Note that there is no line information reported with the error message
// !!! IMPORTANT !!!

DEFINE   F_ERROR := PTR(_CAST,0xFFFFFFFF) // from sysLib\stdlib.prg
GLOBAL   F_ERRORg := PTR(_CAST,0xFFFFFFFF) // OK

DEFINE INVALID_HANDLE_VALUE :=PTR (_CAST, 0xFFFFFFFF)
GLOBAL INVALID_HANDLE_VALUEg :=PTR (_CAST, 0xFFFFFFFF) // ok

DEFINE test := NULL_PTR

FUNCTION Start() AS VOID
? F_ERROR
? test
RETURN

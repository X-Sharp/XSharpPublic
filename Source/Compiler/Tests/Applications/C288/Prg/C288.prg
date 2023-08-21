// 288. error XS0034: Operator '==' is ambiguous on operands of type 'dword' and 'int'
// with /vo4+
#pragma warnings(165, off) // unassigned local
#define INTERNET_ERROR_BASE 12000
#define ERROR_INTERNET_EXTENDED_ERROR (INTERNET_ERROR_BASE + 3)

FUNCTION Start() AS VOID
LOCAL dw AS DWORD
? dw == ERROR_INTERNET_EXTENDED_ERROR


// 293. error XS0221: Constant value '2147483648' cannot be converted to a 'int' (use 'unchecked' syntax to override)
// with /vo4
#define INTERNET_FLAG_RELOAD 0x80000000
#define INTERNET_FLAG_RESYNCHRONIZE 0x00000800

FUNCTION Start( ) AS VOID
LOCAL u := NIL AS USUAL
Default(@u, INTERNET_FLAG_RELOAD + INTERNET_FLAG_RESYNCHRONIZE)
? AsHexString(u)
                                       

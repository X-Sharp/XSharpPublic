// 245. error XS0221: Constant value '2147483648' cannot be converted to a 'int' (use 'unchecked' syntax to override)
#define FTP_TRANSFER_TYPE_BINARY 0x00000002
#define INTERNET_FLAG_RELOAD 0x80000000
FUNCTION Start() AS VOID
? FTP_TRANSFER_TYPE_BINARY + INTERNET_FLAG_RELOAD


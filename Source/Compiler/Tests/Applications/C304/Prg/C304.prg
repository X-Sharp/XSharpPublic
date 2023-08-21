// 304. compiler crash with PSZ
#define LPSTR_TEXTCALLBACK PSZ(_CAST, 0xFFFFFFFF)
FUNCTION Start() AS VOID
LOCAL lpszText AS PSZ
lpszText := LPSTR_TEXTCALLBACK
? (PTR) lpszText

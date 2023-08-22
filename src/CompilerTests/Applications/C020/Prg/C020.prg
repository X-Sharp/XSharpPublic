// 20. compiler crash (escaped strings)
FUNCTION Start() AS VOID
LOCAL c AS STRING
c := e"\r\n"
? c


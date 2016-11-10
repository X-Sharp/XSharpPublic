// 286. error XS0149: Method name expected
FUNCTION Start() AS VOID
? File("c:\test.prg")
LOCAL file AS STRING
file := "c:\test.cfg"
? File(file)


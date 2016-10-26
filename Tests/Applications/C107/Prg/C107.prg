// 107. no error on missing LOCAL type, it gets automatically treated as int
FUNCTION Start() AS VOID
LOCAL c
c := "123"
? c

// 18. compiler crash
FUNCTION Start() AS VOID

LOCAL c1 ,c2 AS STRING // note that both should be assigned of STRING type
c1 := "a"
c2 := c1
? c2

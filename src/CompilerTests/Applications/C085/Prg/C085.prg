// 85. error XS9002: Parser error: no viable alternative at input '(ac,'
FUNCTION Start() AS VOID
LOCAL ac AS STRING[]
ac := <STRING>{"1","2","3"}
? System.Array.IndexOf<STRING>(ac,"2")


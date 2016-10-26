// 82. error XS1061: 'string' does not contain a definition for 'Chars' and no extension ...
FUNCTION Start() AS VOID
LOCAL c AS STRING
c := "123"
? c:Chars[1]


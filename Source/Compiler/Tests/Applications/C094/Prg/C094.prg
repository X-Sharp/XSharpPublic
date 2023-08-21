// 94. error XS0019: Operator '>' cannot be applied to operands of type 'string' and 'string'

// vulcan translates that to a call to String.Compare()
FUNCTION Start() AS VOID
LOCAL c1,c2 AS STRING
c1 := "a"
c2 := "b"
? c1 > c2


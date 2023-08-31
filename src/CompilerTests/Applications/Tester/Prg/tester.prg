FUNCTION STart as void
LOCAL a, b AS FLOAT


a := 80.85m
b := 7.7m
? a / b
? Round(a / b, 0)
? Math.Round( a/b, 0, MidpointRounding.AwayFromZero )
_wait()

